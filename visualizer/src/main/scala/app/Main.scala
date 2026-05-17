package app

import monads.{IO, Reader, Writer, Monoid}
import monads.Monoid.given
import domain.*

/** Точка входа визуализатора аудио.
  *
  * Использование:
  *   sbt "run report.json track.wav"             — путь к отчёту и WAV
  *   sbt "run report.json track.wav --mode bars" — другой режим визуализации
  *   sbt "run report.json track.wav --width 100 --height 25"
  */
@main def run(args: String*): Unit =
  parseArgs(args.toList) match
    case Left(error) =>
      println(s"Ошибка: $error")
      println(usage)
    case Right((cfg, mode)) =>
      visualize(cfg, mode).unsafeRun()

def usage: String =
  """Использование:
    |  sbt "run <report.json> <track.wav> [опции]"
    |
    |Опции:
    |  --mode <spectrum|bars|spectrogram>   режим визуализации (по умолчанию spectrum)
    |  --width <число>                      ширина в символах (по умолчанию 80)
    |  --height <число>                     высота в строках (по умолчанию 20)
    |  --fps <число>                        частота кадров (по умолчанию 30)
    |""".stripMargin

def parseArgs(args: List[String]): Either[String, (RenderConfig, VisualMode)] =
  args match
    case Nil => Right((RenderConfig(reportPath = "", wavPath = ""), VisualMode.Spectrum))
    case report :: wav :: rest if !report.startsWith("--") && !wav.startsWith("--") =>
      parseOptions(rest, RenderConfig(reportPath = report, wavPath = wav), VisualMode.Spectrum)
    case _ =>
      Left("первыми двумя аргументами должны быть путь к JSON-отчёту и путь к WAV")

def parseOptions(
    args: List[String],
    cfg: RenderConfig,
    mode: VisualMode
): Either[String, (RenderConfig, VisualMode)] =
  args match
    case Nil => Right((cfg, mode))
    case "--mode" :: v :: rest =>
      v.toLowerCase match
        case "spectrum"    => parseOptions(rest, cfg, VisualMode.Spectrum)
        case "bars"        => parseOptions(rest, cfg, VisualMode.Bars3)
        case "spectrogram" => parseOptions(rest, cfg, VisualMode.Spectrogram)
        case other         => Left(s"неизвестный режим: $other")
    case "--width" :: v :: rest =>
      v.toIntOption match
        case Some(n) if n > 10 => parseOptions(rest, cfg.copy(width = n), mode)
        case _ => Left(s"--width требует число больше 10 (получено: $v)")
    case "--height" :: v :: rest =>
      v.toIntOption match
        case Some(n) if n > 5 => parseOptions(rest, cfg.copy(height = n), mode)
        case _ => Left(s"--height требует число больше 5 (получено: $v)")
    case "--fps" :: v :: rest =>
      v.toIntOption match
        case Some(n) if n > 0 => parseOptions(rest, cfg.copy(fps = n), mode)
        case _ => Left(s"--fps требует положительное число (получено: $v)")
    case unknown :: _ => Left(s"неизвестная опция: $unknown")

/** Полный сценарий визуализации с использованием всех 4 монад */
def visualize(initialCfg: RenderConfig, initialMode: VisualMode): IO[Unit] =
  for
    cfg          <- ensurePaths(initialCfg)
    _            <- IO.println("=== Визуализатор аудио ===")
    _            <- IO.println(s"Отчёт: ${cfg.reportPath}")
    _            <- IO.println(s"Аудио: ${cfg.wavPath}")

    // Шаг 1: чтение отчёта (IO + Writer)
    reportResult <- ReportReader.read(cfg.reportPath)
    _            <- printLog(reportResult.log)
    report        = reportResult.value

    // Шаг 2: проверка соответствия отчёта и аудиофайла
    _            <- checkConsistency(report, cfg)

    _            <- IO.println("Запуск воспроизведения и визуализации...")
    _            <- IO.println("(анимация остановится по окончании трека)")
    _            <- IO.delay(Thread.sleep(800))

    // Шаг 3: запуск анимации через Reader[RenderConfig, IO[Unit]]
    // Внутри Animation используется State[AnimationState, ...] для управления состоянием
    _            <- Animation.run(report).run(applyMode(cfg, initialMode))
  yield ()

def applyMode(cfg: RenderConfig, mode: VisualMode): RenderConfig =
  // Режим уже передаётся через AnimationState.initial, но если хочешь -
  // можно вынести в cfg. Пока оставим в AnimationState.
  cfg

def ensurePaths(cfg: RenderConfig): IO[RenderConfig] =
  if cfg.reportPath.nonEmpty && cfg.wavPath.nonEmpty then IO.pure(cfg)
  else
    for
      _      <- IO.println("Введите путь к JSON-отчёту:")
      report <- IO.readLine
      _      <- IO.println("Введите путь к WAV-файлу:")
      wav    <- IO.readLine
    yield cfg.copy(reportPath = report.trim, wavPath = wav.trim)

def checkConsistency(report: Report, cfg: RenderConfig): IO[Unit] =
  IO.delay {
    if report.bands.isEmpty then
      println("ВНИМАНИЕ: в отчёте нет данных диапазонов — визуализация будет пустой")
    val expectedDuration = report.source.durationSeconds
    println(f"  Ожидаемая длительность: $expectedDuration%.2f сек")
  }

def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(())) { (acc, line) =>
    for
      _ <- acc
      _ <- IO.println(s"  [log] $line")
    yield ()
  }
