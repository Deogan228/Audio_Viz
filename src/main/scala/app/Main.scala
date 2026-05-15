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
    wavResult   <- WavReader.read(config.filePath)
    _           <- printLog(wavResult.log)
    wav          = wavResult.value

    frames       = analyzeFramesReader.run(config)(wav)
    _           <- IO.println(s"FFT: получено ${frames.length} кадров")

    beatResult   = BeatDetector.detectAll(frames, config.beatThreshold, config.beatWindow)
    _           <- printLog(beatResult.log)
    (beats, bpm) = beatResult.value

    result       = AnalysisResult(wav.header, frames, beats, bpm)
    _           <- IO.println(Visualizer.summary(result))

    _           <- IO.println("Показать визуализацию? (y/n)")
    answer      <- IO.readLine
    _           <- runAnimationIfNeeded(answer, frames)
  yield ()

def runAnimationIfNeeded(answer: String, frames: Vector[SpectrumFrame]): IO[Unit] =
  if answer.trim.toLowerCase == "y" then
    Visualizer.animate(frames, fps = 20, height = 20, width = 60)
  else
    IO.println("Готово.")

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