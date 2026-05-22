package ui

import domain.{AnalysisPipeline, Config}

import zio.{ZIO, Runtime, Unsafe, Ref}

import java.awt.BorderLayout
import java.io.File
import javax.swing.{JFrame, JButton, JPanel, JLabel, JFileChooser, SwingUtilities, WindowConstants}
import javax.swing.filechooser.FileNameExtensionFilter

/** Главное окно анализатора — графический "интерфейс на view".
  *
  * Связывает Swing-события с ZIO-конвейером:
  *   - нажатие кнопки запускает ZIO-эффект анализа;
  *   - результат отдаётся в SpectrumPanel для отрисовки;
  *   - флаг "идёт анализ" хранится в zio.Ref — это ZIO-замена
  *     глобальной изменяемой переменной (блок 3 ТЗ: "если хочется
  *     глобальную переменную — используем State/Ref").
  *
  * Сами Swing-вызовы — побочные эффекты, поэтому они обёрнуты в
  * ZIO.attempt и исполняются на EDT через SwingUtilities.invokeLater.
  */
object AnalyzerWindow:

  private val runtime = Runtime.default

  /** Запустить любой ZIO-эффект "из мира Swing" (вне ZIO App). */
  private def unsafeRunAsync[E, A](effect: ZIO[Any, E, A]): Unit =
    Unsafe.unsafe { implicit u =>
      val _ = runtime.unsafe.fork(effect)
      ()
    }

  /** Построить и показать окно. Эффект — создание окна это побочное
    * действие, поэтому возвращаем ZIO[Any, Throwable, Unit].
    */
  def open(initial: Config): ZIO[Any, Throwable, Unit] =
    for
      // Ref как "глобальная переменная" состояния UI (блок 3).
      busyRef  <- Ref.make(false)
      configRef <- Ref.make(initial)
      _        <- ZIO.attempt(SwingUtilities.invokeLater(() => build(initial, busyRef, configRef)))
      // Держим ZIO-волокно живым, пока открыто окно.
      _        <- ZIO.never
    yield ()

  private def build(initial: Config, busyRef: Ref[Boolean], configRef: Ref[Config]): Unit =
    val frame = new JFrame("Анализатор аудио — ZIO + Swing")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val panel = new SpectrumPanel
    val status = new JLabel("Готов. Откройте WAV-файл.")
    val openBtn = new JButton("Открыть WAV…")

    openBtn.addActionListener { _ =>
      val chooser = new JFileChooser()
      chooser.setFileFilter(new FileNameExtensionFilter("WAV-файлы", "wav"))
      val res = chooser.showOpenDialog(frame)
      if res == JFileChooser.APPROVE_OPTION then
        val file: File = chooser.getSelectedFile
        startAnalysis(file, panel, status, openBtn, busyRef, configRef)
    }

    val top = new JPanel(new BorderLayout())
    top.add(openBtn, BorderLayout.WEST)
    top.add(status, BorderLayout.CENTER)

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
      busyRef: Ref[Boolean],
      configRef: Ref[Config]
  ): Unit =
    /** Обновление Swing-компонентов всегда на EDT. */
    def onEdt(action: => Unit): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(SwingUtilities.invokeLater(() => action))

    val effect: ZIO[Any, Throwable, Unit] =
      for
        alreadyBusy <- busyRef.getAndSet(true)
        _ <- ZIO.unless(alreadyBusy) {
               for
                 _   <- onEdt {
                          openBtn.setEnabled(false)
                          status.setText(s"Анализ: ${file.getName}…")
                        }
                 cfg <- configRef.updateAndGet(_.copy(filePath = file.getAbsolutePath))
                 // Конвейер требует Config в окружении — поставляем через ZLayer.
                 outcome <- AnalysisPipeline.run.provide(Config.layer(cfg))
                 _   <- onEdt {
                          panel.show(outcome)
                          status.setText(
                            f"Готово: ${file.getName}  —  BPM ${outcome.result.bpm}%.1f, " +
                            s"битов ${outcome.result.beats.length}"
                          )
                        }
               yield ()
             }
      yield ()

    // Ошибки конвейера показываем в статусе, busy-флаг всегда снимаем.
    val safe = effect
      .catchAll { err =>
        onEdt(status.setText(s"Ошибка: ${err.getMessage}"))
      }
      .ensuring(
        busyRef.set(false) *> onEdt(openBtn.setEnabled(true))
      )

    unsafeRunAsync(safe)
