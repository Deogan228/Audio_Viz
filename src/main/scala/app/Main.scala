package app

import monads.{IO, Reader, Writer, Monoid}
import monads.Monoid.given
import domain.*

@main def run(): Unit =
  val program: IO[Unit] =
    for
      _    <- IO.println("=== Аудио-визуализатор ===")
      _    <- IO.println("Введите путь к WAV-файлу:")
      path <- IO.readLine
      cfg   = Config(filePath = path)
      _    <- analyze(cfg)
    yield ()

  program.unsafeRun()

def analyze(config: Config): IO[Unit] =
  for
    // Шаг 1: чтение файла (IO + Writer)
    wavResult <- WavReader.read(config.filePath)
    _         <- printLog(wavResult.log)
    wav        = wavResult.value

    // Шаг 2: FFT через Reader[Config, WavData => Vector[SpectrumFrame]]
    frames     = analyzeFramesReader.run(config)(wav)
    _         <- IO.println(s"FFT: получено ${frames.length} кадров")

    // Шаг 3: детекция битов (Writer + State внутри)
    beatResult = BeatDetector.detectAll(frames, config.beatThreshold, config.beatWindow)
    _         <- printLog(beatResult.log)
    (beats, bpm) = beatResult.value

    result     = AnalysisResult(wav.header, frames, beats, bpm)
    _         <- IO.println(Visualizer.summary(result))

    // Шаг 4: анимация по желанию
    _         <- IO.println("Показать визуализацию? (y/n)")
    answer    <- IO.readLine
    _         <- if answer.trim.toLowerCase == "y" then
                   Visualizer.animate(frames, fps = 20, height = 20, width = 60)
                 else
                   IO.println("Готово.")
  yield ()

/** Reader демонстрирует проброс конфига без явной передачи параметров */
def analyzeFramesReader: Reader[Config, WavData => Vector[SpectrumFrame]] =
  Reader.asks { config => wav =>
    FFT.analyze(wav.samples, wav.header.sampleRate, config.fftSize)
  }

def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(())) { (acc, line) =>
    for
      _ <- acc
      _ <- IO.println(s"  [log] $line")
    yield ()
  }