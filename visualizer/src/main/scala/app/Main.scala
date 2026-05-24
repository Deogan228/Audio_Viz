package app

import domain.{Animation, ReportReader, RenderConfig, VisualMode, Report}
import ui.{VisualizerWindow, SpectrumCanvas}

import zio.{ZIO, ZIOAppDefault, ZIOAppArgs, Console, ExitCode, Scope}

import javax.swing.{JFrame, SwingUtilities, WindowConstants}
import java.awt.BorderLayout

// Главный вход в программу
// Смотрим аргументы командной строки: можно сразу указать файлы или открыть окно выбора
// Если ничего не передано — тоже окно выбора
object Main extends ZIOAppDefault:

  def run: ZIO[ZIOAppArgs, Nothing, ExitCode] =
    for
      args     <- ZIOAppArgs.getArgs
      exitCode <- program(args.toList)
    yield exitCode

  private def program(args: List[String]): ZIO[Any, Nothing, ExitCode] =
    val uiFlag = args.contains("--ui")
    val rest   = args.filterNot(_ == "--ui")

    parseArgs(rest) match
      case Left(error) =>
        (Console.printLine(s"Ошибка: $error") *> Console.printLine(usage))
          .as(ExitCode.failure).orDie
      case Right(cfg) =>
        // Если файлы не заданы или явно попросили UI — показываем окно выбора.
        if uiFlag || cfg.reportPath.isEmpty || cfg.wavPath.isEmpty then
          runUi(cfg)
        else
          runDirect(cfg)

  /** Окно с кнопками для выбора файлов */
  private def runUi(cfg: RenderConfig): ZIO[Any, Nothing, ExitCode] =
    VisualizerWindow.open(cfg)
      .as(ExitCode.success)
      .catchAll(err => Console.printLine(s"Ошибка UI: ${err.getMessage}").orDie.as(ExitCode.failure))

  /** Прямой запуск. Файлы уже известны — сразу открываем окно визуализации и стартуем анимацию */
  private def runDirect(cfg: RenderConfig): ZIO[Any, Nothing, ExitCode] =
    val effect: ZIO[Any, Throwable, Unit] =
      for
        _            <- Console.printLine("=== Визуализатор аудио ===")
        _            <- Console.printLine(s"Отчёт: ${cfg.reportPath}")
        _            <- Console.printLine(s"Аудио: ${cfg.wavPath}")
        reportWriter <- ReportReader.read(cfg.reportPath)
        _            <- ZIO.foreachDiscard(reportWriter.log)(l =>
                          Console.printLine(s"  [log] $l"))
        report        = reportWriter.value
        canvas       <- makeWindow()
        _            <- ZIO.scoped {
                          Animation.run(report, canvas)
                            .provideSomeLayer[Scope](RenderConfig.layer(cfg))
                        }
        _            <- Console.printLine("Воспроизведение завершено.")
      yield ()

    effect
      .catchAll(err => Console.printLine(s"Ошибка: ${err.getMessage}").orDie)
      .as(ExitCode.success)

  /** Создать окно с холстом для рисования спектра */
  private def makeWindow(): ZIO[Any, Throwable, SpectrumCanvas] =
    ZIO.attempt {
      val canvas = new SpectrumCanvas
      SwingUtilities.invokeLater { () =>
        val frame = new JFrame("Визуализатор аудио — ZIO + Swing")
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        frame.getContentPane.add(canvas, BorderLayout.CENTER)
        frame.pack()
        frame.setLocationRelativeTo(null)
        frame.setVisible(true)
      }
      canvas
    }

  def usage: String =
    """Использование:
      |  sbt "run <report.json> <track.wav> [опции]"   — окно визуализации
      |  sbt "run --ui"                                — окно с выбором файлов
      |
      |Опции:
      |  --mode <spectrum|bars|spectrogram>   режим визуализации
      |  --width <число>                      ширина окна (по умолчанию 820)
      |  --height <число>                     высота окна (по умолчанию 460)
      |  --fps <число>                        частота кадров (по умолчанию 30)
      |  --ui                                 принудительно открыть выбор файлов
      |""".stripMargin

  def parseArgs(args: List[String]): Either[String, RenderConfig] =
    args match
      case Nil =>
        Right(RenderConfig(reportPath = "", wavPath = ""))
      case report :: wav :: rest if !report.startsWith("--") && !wav.startsWith("--") =>
        parseOptions(rest, RenderConfig(reportPath = report, wavPath = wav))
      case single :: rest if !single.startsWith("--") =>
        // Если указан только один путь — откроем окно выбора.
        parseOptions(rest, RenderConfig(reportPath = single, wavPath = ""))
      case rest =>
        parseOptions(rest, RenderConfig(reportPath = "", wavPath = ""))

  def parseOptions(args: List[String], cfg: RenderConfig): Either[String, RenderConfig] =
    args match
      case Nil => Right(cfg)
      case "--mode" :: v :: rest =>
        v.toLowerCase match
          case "spectrum"    => parseOptions(rest, cfg.copy(mode = VisualMode.Spectrum))
          case "bars"        => parseOptions(rest, cfg.copy(mode = VisualMode.Bars3))
          case "spectrogram" => parseOptions(rest, cfg.copy(mode = VisualMode.Spectrogram))
          case other         => Left(s"неизвестный режим: $other")
      case "--width" :: v :: rest =>
        v.toIntOption match
          case Some(n) if n > 100 => parseOptions(rest, cfg.copy(width = n))
          case _ => Left(s"--width требует число больше 100 (получено: $v)")
      case "--height" :: v :: rest =>
        v.toIntOption match
          case Some(n) if n > 100 => parseOptions(rest, cfg.copy(height = n))
          case _ => Left(s"--height требует число больше 100 (получено: $v)")
      case "--fps" :: v :: rest =>
        v.toIntOption match
          case Some(n) if n > 0 => parseOptions(rest, cfg.copy(fps = n))
          case _ => Left(s"--fps требует положительное число (получено: $v)")
      case unknown :: _ => Left(s"неизвестная опция: $unknown")
