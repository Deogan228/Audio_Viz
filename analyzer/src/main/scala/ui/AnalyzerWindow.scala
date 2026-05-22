package ui

import domain.{AnalysisPipeline, Config, Report}

import zio.{ZIO, Runtime, Unsafe, Ref}

import java.awt.BorderLayout
import java.io.File
import javax.swing.{JFrame, JButton, JPanel, JLabel, JFileChooser, SwingUtilities, WindowConstants}
import javax.swing.filechooser.FileNameExtensionFilter

/** Главное окно анализатора — графический "интерфейс на view".
  *
  * Связывает Swing-события с ZIO-конвейером:
  *   - кнопка "Открыть WAV" запускает ZIO-эффект анализа;
  *   - результат отдаётся в SpectrumPanel для отрисовки;
  *   - кнопка "Сохранить отчёт" пишет JSON/текст на диск;
  *   - флаг "идёт анализ" и последний результат хранятся в zio.Ref
  *     (блок 3 ТЗ: Ref как замена глобальной изменяемой переменной).
  */
object AnalyzerWindow:

  private val runtime = Runtime.default

  private def unsafeRunAsync[E, A](effect: ZIO[Any, E, A]): Unit =
    Unsafe.unsafe { implicit u =>
      val _ = runtime.unsafe.fork(effect)
      ()
    }

  def open(initial: Config): ZIO[Any, Throwable, Unit] =
    for
      busyRef    <- Ref.make(false)
      configRef  <- Ref.make(initial)
      // последний результат анализа — для кнопки "Сохранить"
      outcomeRef <- Ref.make(Option.empty[AnalysisPipeline.Outcome])
      _          <- ZIO.attempt(SwingUtilities.invokeLater(() =>
                      build(initial, busyRef, configRef, outcomeRef)))
      _          <- ZIO.never
    yield ()

  private def build(
      initial: Config,
      busyRef: Ref[Boolean],
      configRef: Ref[Config],
      outcomeRef: Ref[Option[AnalysisPipeline.Outcome]]
  ): Unit =
    val frame = new JFrame("Анализатор аудио — ZIO + Swing")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val panel = new SpectrumPanel
    val status = new JLabel("Готов. Откройте WAV-файл.")
    val openBtn = new JButton("Открыть WAV…")
    val saveBtn = new JButton("Сохранить отчёт…")
    saveBtn.setEnabled(false) // активна только после успешного анализа

    openBtn.addActionListener { _ =>
      val chooser = new JFileChooser()
      chooser.setFileFilter(new FileNameExtensionFilter("WAV-файлы", "wav"))
      val res = chooser.showOpenDialog(frame)
      if res == JFileChooser.APPROVE_OPTION then
        val file: File = chooser.getSelectedFile
        startAnalysis(file, panel, status, openBtn, saveBtn, busyRef, configRef, outcomeRef)
    }

    saveBtn.addActionListener { _ =>
      saveReport(frame, status, outcomeRef, configRef)
    }

    val controls = new JPanel()
    controls.add(openBtn)
    controls.add(saveBtn)

    val top = new JPanel(new BorderLayout())
    top.add(controls, BorderLayout.WEST)
    top.add(status, BorderLayout.SOUTH)

    frame.getContentPane.add(top, BorderLayout.NORTH)
    frame.getContentPane.add(panel, BorderLayout.CENTER)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)

  /** Запуск анализа выбранного файла через ZIO-конвейер. */
  private def startAnalysis(
      file: File,
      panel: SpectrumPanel,
      status: JLabel,
      openBtn: JButton,
      saveBtn: JButton,
      busyRef: Ref[Boolean],
      configRef: Ref[Config],
      outcomeRef: Ref[Option[AnalysisPipeline.Outcome]]
  ): Unit =
    def onEdt(action: => Unit): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(SwingUtilities.invokeLater(() => action))

    val effect: ZIO[Any, Throwable, Unit] =
      for
        alreadyBusy <- busyRef.getAndSet(true)
        _ <- ZIO.unless(alreadyBusy) {
               for
                 _   <- onEdt {
                          openBtn.setEnabled(false)
                          saveBtn.setEnabled(false)
                          status.setText(s"Анализ: ${file.getName}…")
                        }
                 cfg <- configRef.updateAndGet(_.copy(filePath = file.getAbsolutePath))
                 outcome <- AnalysisPipeline.run.provide(Config.layer(cfg))
                 _   <- outcomeRef.set(Some(outcome))
                 _   <- onEdt {
                          panel.show(outcome)
                          saveBtn.setEnabled(true)
                          status.setText(
                            f"Готово: ${file.getName}  —  BPM ${outcome.result.bpm}%.1f, " +
                            s"битов ${outcome.result.beats.length}"
                          )
                        }
               yield ()
             }
      yield ()

    val safe = effect
      .catchAll(err => onEdt(status.setText(s"Ошибка: ${err.getMessage}")))
      .ensuring(busyRef.set(false) *> onEdt(openBtn.setEnabled(true)))

    unsafeRunAsync(safe)

  /** Сохранение последнего результата анализа в JSON и текст. */
  private def saveReport(
      frame: JFrame,
      status: JLabel,
      outcomeRef: Ref[Option[AnalysisPipeline.Outcome]],
      configRef: Ref[Config]
  ): Unit =
    def onEdt(action: => Unit): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(SwingUtilities.invokeLater(() => action))

    // Диалог выбора пути для JSON — синхронно, в EDT (обработчик кнопки).
    val chooser = new JFileChooser()
    chooser.setFileFilter(new FileNameExtensionFilter("JSON-отчёты", "json"))
    chooser.setSelectedFile(new File("report.json"))
    val res = chooser.showSaveDialog(frame)
    if res != JFileChooser.APPROVE_OPTION then ()
    else
      val jsonFile = chooser.getSelectedFile
      val jsonPath = jsonFile.getAbsolutePath
      // текстовый отчёт кладём рядом, с тем же именем и расширением .txt
      val textPath =
        if jsonPath.toLowerCase.endsWith(".json")
        then jsonPath.dropRight(5) + ".txt"
        else jsonPath + ".txt"

      val effect: ZIO[Any, Throwable, Unit] =
        for
          maybeOutcome <- outcomeRef.get
          _ <- maybeOutcome match
                 case None =>
                   onEdt(status.setText("Нечего сохранять — сначала проанализируйте файл"))
                 case Some(outcome) =>
                   for
                     _ <- domain.Report.save(outcome.jsonContent, jsonPath)
                     _ <- domain.Report.save(outcome.textContent, textPath)
                     _ <- onEdt(status.setText(s"Отчёт сохранён: $jsonPath"))
                   yield ()
        yield ()

      val safe = effect.catchAll(err =>
        onEdt(status.setText(s"Ошибка сохранения: ${err.getMessage}")))

      unsafeRunAsync(safe)