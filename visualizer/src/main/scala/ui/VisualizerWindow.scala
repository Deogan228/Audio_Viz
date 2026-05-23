package ui

import domain.{Animation, ReportReader, RenderConfig, Report}

import zio.{ZIO, Runtime, Unsafe, Ref, Scope}

import java.awt.BorderLayout
import java.io.File
import javax.swing.{JFrame, JButton, JPanel, JLabel, JFileChooser, SwingUtilities, WindowConstants}
import javax.swing.filechooser.FileNameExtensionFilter

/** Главное окно визуализатора — графический "интерфейс на view".
  *
  * Связывает Swing-события с ZIO:
  *   - кнопки выбирают JSON-отчёт и WAV-файл;
  *   - кнопка "Запустить" стартует ZIO-цикл Animation, который проигрывает
  *     звук и синхронно кормит SpectrumCanvas кадрами;
  *   - флаг "идёт визуализация" хранится в zio.Ref (блок 3: Ref как
  *     замена глобальной переменной).
  *
  * Swing-вызовы — побочные эффекты, поэтому обёрнуты в ZIO.attempt и
  * исполняются на EDT через SwingUtilities.invokeLater.
  */
object VisualizerWindow:

  private val runtime = Runtime.default

  /** Запустить ZIO-эффект "из мира Swing" (вне ZIO App). */
  private def unsafeRunAsync[E, A](effect: ZIO[Any, E, A]): Unit =
    Unsafe.unsafe { implicit u =>
      val _ = runtime.unsafe.fork(effect)
      ()
    }

  /** Построить и показать окно. */
  def open(initial: RenderConfig): ZIO[Any, Throwable, Unit] =
    for
      busyRef   <- Ref.make(false)
      configRef <- Ref.make(initial)
      _         <- ZIO.attempt(SwingUtilities.invokeLater(() => build(initial, busyRef, configRef)))
      _         <- ZIO.never
    yield ()

  private def build(initial: RenderConfig, busyRef: Ref[Boolean], configRef: Ref[RenderConfig]): Unit =
    val frame = new JFrame("Визуализатор аудио — ZIO + Swing")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val canvas = new SpectrumCanvas
    val status = new JLabel("Выберите JSON-отчёт и WAV-файл.")

    val jsonBtn  = new JButton("Отчёт JSON…")
    val wavBtn   = new JButton("WAV-файл…")
    val startBtn = new JButton("Запустить")

    if initial.reportPath.nonEmpty then jsonBtn.setText(s"Отчёт: ${shortName(initial.reportPath)}")
    if initial.wavPath.nonEmpty then wavBtn.setText(s"WAV: ${shortName(initial.wavPath)}")

    jsonBtn.addActionListener { _ =>
      chooseFile(frame, "JSON-отчёты", "json").foreach { f =>
        unsafeRunAsync(configRef.update(_.copy(reportPath = f.getAbsolutePath)))
        jsonBtn.setText(s"Отчёт: ${f.getName}")
      }
    }

    wavBtn.addActionListener { _ =>
      chooseFile(frame, "WAV-файлы", "wav").foreach { f =>
        unsafeRunAsync(configRef.update(_.copy(wavPath = f.getAbsolutePath)))
        wavBtn.setText(s"WAV: ${f.getName}")
      }
    }

    startBtn.addActionListener { _ =>
      startVisualization(canvas, status, startBtn, busyRef, configRef)
    }

    val controls = new JPanel()
    controls.add(jsonBtn)
    controls.add(wavBtn)
    controls.add(startBtn)

    val top = new JPanel(new BorderLayout())
    top.add(controls, BorderLayout.WEST)
    top.add(status, BorderLayout.SOUTH)

    frame.getContentPane.add(top, BorderLayout.NORTH)
    frame.getContentPane.add(canvas, BorderLayout.CENTER)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)

  /** Запуск ZIO-цикла визуализации для выбранных файлов. */
  private def startVisualization(
      canvas: SpectrumCanvas,
      status: JLabel,
      startBtn: JButton,
      busyRef: Ref[Boolean],
      configRef: Ref[RenderConfig]
  ): Unit =
    def onEdt(action: => Unit): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(SwingUtilities.invokeLater(() => action))

    val effect: ZIO[Any, Throwable, Unit] =
      for
        cfg         <- configRef.get
        _           <- validate(cfg)
        alreadyBusy <- busyRef.getAndSet(true)
        _ <- ZIO.unless(alreadyBusy) {
               for
                 _            <- onEdt {
                                   startBtn.setEnabled(false)
                                   status.setText("Загрузка отчёта…")
                                 }
                 reportWriter <- ReportReader.read(cfg.reportPath)
                 // Writer (блок 2): разворачиваем лог загрузки в консоль.
                 _            <- ZIO.foreachDiscard(reportWriter.log)(l =>
                                   zio.Console.printLine(s"  [log] $l").orDie)
                 report        = reportWriter.value
                 _            <- onEdt(status.setText(
                                   s"Воспроизведение: ${shortName(cfg.wavPath)}  " +
                                   s"(${report.bands.length} кадров, BPM ${"%.1f".format(report.analysis.bpm)})"))
                 // Animation требует RenderConfig (Reader→ZIO) и Scope (ресурс аудио).
                 _            <- runAnimation(report, canvas, cfg)
                 _            <- onEdt(status.setText("Готово. Воспроизведение завершено."))
               yield ()
             }
      yield ()

    val safe = effect
      .catchAll(err => onEdt(status.setText(s"Ошибка: ${err.getMessage}")))
      .ensuring(busyRef.set(false) *> onEdt(startBtn.setEnabled(true)))

    unsafeRunAsync(safe)

  /** Запустить цикл анимации, предоставив окружение и Scope.
    * Сначала поставляем RenderConfig (Reader→ZIO), затем ZIO.scoped
    * закрывает оставшееся требование Scope и гарантирует освобождение
    * аудио-ресурса по завершении.
    */
  private def runAnimation(
      report: Report,
      canvas: SpectrumCanvas,
      cfg: RenderConfig
  ): ZIO[Any, Throwable, Unit] =
    ZIO.scoped {
      Animation.run(report, canvas).provideSomeLayer[Scope](RenderConfig.layer(cfg))
    }

  private def validate(cfg: RenderConfig): ZIO[Any, Throwable, Unit] =
    if cfg.reportPath.isEmpty then
      ZIO.fail(new RuntimeException("не выбран JSON-отчёт"))
    else if cfg.wavPath.isEmpty then
      ZIO.fail(new RuntimeException("не выбран WAV-файл"))
    else ZIO.unit

  private def chooseFile(parent: JFrame, desc: String, ext: String): Option[File] =
    val chooser = new JFileChooser()
    chooser.setFileFilter(new FileNameExtensionFilter(desc, ext))
    if chooser.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION then
      Some(chooser.getSelectedFile)
    else None

  private def shortName(path: String): String =
    val f = new File(path)
    f.getName
