package app

import monads.{IO, Reader, Writer, Monoid}
import monads.Monoid.given
import domain.*

/** Точка входа анализатора аудио.
  *
  * Использование:
  *   sbt "run track.wav"                                — анализ с настройками по умолчанию
  *   sbt "run track.wav --fft 2048 --threshold 1.5"     — с параметрами
  *   sbt "run track.wav --json my.json --text my.txt"   — свои пути отчётов
  */
@main def run(args: String*): Unit =
  parseArgs(args.toList) match
    case Left(error) =>
      println(s"Ошибка: $error")
      println(usage)
    case Right(cfg) =>
      analyze(cfg).unsafeRun()

def usage: String =
  """Использование:
    |  sbt "run <путь_к_wav> [опции]"
    |
    |Опции:
    |  --fft <число>         размер окна FFT (по умолчанию 1024)
    |  --threshold <число>   порог детекции битов (по умолчанию 1.3)
    |  --window <число>      окно усреднения энергии (по умолчанию 43)
    |  --json <путь>         путь к JSON-отчёту (по умолчанию report.json)
    |  --text <путь>         путь к текстовому отчёту (по умолчанию report.txt)
    |""".stripMargin

def parseArgs(args: List[String]): Either[String, Config] =
  args match
    case Nil =>
      Right(Config(filePath = ""))
    case path :: rest if !path.startsWith("--") =>
      parseOptions(rest, Config(filePath = path))
    case _ =>
      Left("первым аргументом должен быть путь к WAV-файлу")

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

/** Полный конвейер анализа с использованием всех 4 монад */
def analyze(initialConfig: Config): IO[Unit] =
  for
    cfg          <- ensurePath(initialConfig)
    _            <- IO.println(s"=== Анализатор аудио ===")
    _            <- IO.println(s"Файл: ${cfg.filePath}")

    wavResult    <- WavReader.read(cfg.filePath)
    _            <- printLog(wavResult.log)
    wav           = wavResult.value

    frames        = fftReader.run(cfg)(wav)
    _            <- IO.println(s"FFT: ${frames.length} кадров")

    beatResult    = BeatDetector.detectAll(frames, cfg.beatThreshold, cfg.beatWindow)
    _            <- printLog(beatResult.log)
    (beats, bpm)  = beatResult.value

    bands         = BandAnalyzer.analyzeAll(frames).run(cfg)
    bandSummary   = BandAnalyzer.summary(bands)
    _            <- printLog(bandSummary.log)

    result        = AnalysisResult(wav.header, frames, beats, bpm)
    jsonContent   = Report.buildJson(result, bands).run(cfg)
    textContent   = Report.buildText(result, bandSummary.value).run(cfg)

    _            <- Report.save(jsonContent, cfg.outputJsonPath)
    _            <- IO.println(s"JSON-отчёт сохранён: ${cfg.outputJsonPath}")
    _            <- Report.save(textContent, cfg.outputTextPath)
    _            <- IO.println(s"Текстовый отчёт сохранён: ${cfg.outputTextPath}")

    _            <- IO.println("")
    _            <- IO.println(textContent)
  yield ()

def ensurePath(cfg: Config): IO[Config] =
  if cfg.filePath.nonEmpty then IO.pure(cfg)
  else
    for
      _    <- IO.println("Введите путь к WAV-файлу:")
      path <- IO.readLine
    yield cfg.copy(filePath = path.trim)

def fftReader: Reader[Config, WavData => Vector[SpectrumFrame]] =
  Reader.asks { cfg => wav =>
    FFT.analyze(wav.samples, wav.header.sampleRate, cfg.fftSize)
  }

def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(())) { (acc, line) =>
    for
      _ <- acc
      _ <- IO.println(s"  [log] $line")
    yield ()
  }