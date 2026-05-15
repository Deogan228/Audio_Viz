package app

import monads.{IO, Reader, Writer, Monoid}
import monads.Monoid.given
import domain.*

@main def run(): Unit =
  val program: IO[Unit] =
    for
      _      <- IO.println("=== Аудио-визуализатор ===")
      _      <- IO.println("Введите путь к WAV-файлу:")
      path   <- IO.readLine
      config = Config(filePath = path)
      _      <- analyze(config)
    yield ()

  program.unsafeRun()

  /** Полный конвейер анализа.
    * Reader пробрасывает Config во все вычисления, которые его требуют.
    */
  def analyze(config: Config): IO[Unit] =
    // Шаг 1: чтение файла (IO + Writer)
    for
      wavResult <- WavReader.read(config.filePath)
      _         <- printLog(wavResult.log)

      wav = wavResult.value

      // Шаг 2: FFT-анализ через Reader[Config, ...]
      framesReader = analyzeFramesReader
      frames       = framesReader.run(config).run(wav)

      _ <- IO.println(s"FFT: получено ${frames.length} кадров")

      // Шаг 3: детекция битов (Writer + State внутри)
      beatResult = BeatDetector.detectAll(frames, config.beatThreshold, config.beatWindow)
      _         <- printLog(beatResult.log)

      (beats, bpm) = beatResult.value

      result = AnalysisResult(wav.header, frames, beats, bpm)
      _ <- IO.println(Visualizer.summary(result))

      // Шаг 4: спрашиваем пользователя, показывать ли анимацию
      _    <- IO.println("Показать визуализацию? (y/n)")
      answer <- IO.readLine
      _ <- if answer.trim.toLowerCase == "y" then
              Visualizer.animate(frames, fps = 20, height = 20, width = 60)
           else
              IO.println("Готово.")
    yield ()

  /** Reader[Config, WavData => Vector[SpectrumFrame]]
    * Демонстрирует Reader: функция зависит от config, не получает его явно.
    */
  def analyzeFramesReader: Reader[Config, WavData => Vector[SpectrumFrame]] =
    Reader.asks { config => wav =>
      FFT.analyze(wav.samples, wav.header.sampleRate, config.fftSize)
    }

  /** Утилита: печатает каждую строку лога */
  def printLog(log: Vector[String]): IO[Unit] =
    log.foldLeft(IO.pure(())) { (acc, line) =>
      acc.flatMap(_ => IO.println(s"  [log] $line"))
    }