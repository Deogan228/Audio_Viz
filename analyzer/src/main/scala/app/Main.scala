package app

import domain.{AnalysisPipeline, Config}
import ui.AnalyzerWindow

import zio.{ZIO, ZIOAppDefault, ZIOAppArgs, Console, ExitCode}

/** Точка входа анализатора аудио.
  *
  * Блок 4 (IO → ZIO): весь сценарий — это ZIO-программа, запускаемая
  * через ZIOAppDefault. Наивный IO остаётся только в monads/ как
  * учебная демонстрация блока 0.
  *
  * Использование:
  *   sbt "run track.wav"                              — анализ в консоли
  *   sbt "run track.wav --fft 2048 --threshold 1.5"   — с параметрами
  *   sbt "run --ui"                                   — графическое окно
  *   sbt "run --ui track.wav"                         — окно + сразу файл
  *   sbt run                                          — спросит путь
  */
object Main extends ZIOAppDefault:

  def run: ZIO[ZIOAppArgs, Nothing, ExitCode] =
    for
      args     <- ZIOAppArgs.getArgs
      exitCode <- program(args.toList)
    yield exitCode

  private def program(args: List[String]): ZIO[Any, Nothing, ExitCode] =
    val uiMode = args.contains("--ui")
    val rest   = args.filterNot(_ == "--ui")

    parseArgs(rest) match
      case Left(error) =>
        (Console.printLine(s"Ошибка: $error") *> Console.printLine(usage))
          .as(ExitCode.failure)
          .orDie
      case Right(cfg) =>
        if uiMode then runUi(cfg)
        else runCli(cfg)

  /** Графический режим: открываем окно. */
  private def runUi(cfg: Config): ZIO[Any, Nothing, ExitCode] =
    AnalyzerWindow.open(cfg)
      .as(ExitCode.success)
      .catchAll(err => Console.printLine(s"Ошибка UI: ${err.getMessage}").orDie.as(ExitCode.failure))

  /** Консольный режим: при необходимости спрашиваем путь, затем анализ. */
  private def runCli(initial: Config): ZIO[Any, Nothing, ExitCode] =
    val effect =
      for
        cfg <- ensurePath(initial)
        _   <- AnalysisPipeline.runAndSave.provide(Config.layer(cfg))
      yield ExitCode.success

    effect.catchAll { err =>
      Console.printLine(s"Ошибка анализа: ${err.getMessage}").orDie.as(ExitCode.failure)
    }

  /** Если путь не задан в аргументах — спрашиваем у пользователя (блок 4). */
  private def ensurePath(cfg: Config): ZIO[Any, Throwable, Config] =
    if cfg.filePath.nonEmpty then ZIO.succeed(cfg)
    else
      for
        _    <- Console.printLine("Введите путь к WAV-файлу:")
        path <- Console.readLine
      yield cfg.copy(filePath = path.trim)

  def usage: String =
    """Использование:
      |  sbt "run <путь_к_wav> [опции]"     — анализ в консоли
      |  sbt "run --ui [путь_к_wav]"        — графическое окно
      |
      |Опции:
      |  --fft <число>         размер окна FFT (по умолчанию 1024)
      |  --threshold <число>   порог детекции битов (по умолчанию 1.3)
      |  --window <число>      окно усреднения энергии (по умолчанию 43)
      |  --json <путь>         путь к JSON-отчёту (по умолчанию report.json)
      |  --text <путь>         путь к текстовому отчёту (по умолчанию report.txt)
      |  --ui                  открыть графический интерфейс
      |""".stripMargin

  def parseArgs(args: List[String]): Either[String, Config] =
    args match
      case Nil =>
        Right(Config(filePath = ""))
      case path :: rest if !path.startsWith("--") =>
        parseOptions(rest, Config(filePath = path))
      case rest =>
        // путь не задан, но есть только опции — допустимо (спросим путь)
        parseOptions(rest, Config(filePath = ""))

  def parseOptions(args: List[String], cfg: Config): Either[String, Config] =
    args match
      case Nil => Right(cfg)
      case "--fft" :: v :: rest =>
        v.toIntOption match
          case Some(n) if isPow2(n) => parseOptions(rest, cfg.copy(fftSize = n))
          case _ => Left(s"--fft требует число, степень двойки (получено: $v)")
      case "--threshold" :: v :: rest =>
        v.toDoubleOption match
          case Some(d) => parseOptions(rest, cfg.copy(beatThreshold = d))
          case None => Left(s"--threshold требует число (получено: $v)")
      case "--window" :: v :: rest =>
        v.toIntOption match
          case Some(n) => parseOptions(rest, cfg.copy(beatWindow = n))
          case None => Left(s"--window требует число (получено: $v)")
      case "--json" :: v :: rest => parseOptions(rest, cfg.copy(outputJsonPath = v))
      case "--text" :: v :: rest => parseOptions(rest, cfg.copy(outputTextPath = v))
      case unknown :: _ => Left(s"неизвестная опция: $unknown")

  def isPow2(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0
