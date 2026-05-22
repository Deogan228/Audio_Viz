package domain

import zio.{ZIO, URIO, Console}

/** Полный конвейер анализа на ZIO.
  *
  * Вынесен отдельно, чтобы его переиспользовали и консольный сценарий
  * (app/Main.scala), и оконный интерфейс (ui/AnalyzerWindow.scala).
  *
  * Здесь видно, как уживаются ZIO и собственные монады:
  *   - WavReader.read, Report.save     — ZIO-эффекты (блок 4);
  *   - BandAnalyzer.analyzeAll, Report — ZIO-окружение Config (блок 1);
  *   - BeatDetector.detectAll, summary — наивный Writer (блок 2),
  *     лог которого мы разворачиваем в ZIO-консоль методом emitLog;
  *   - BeatDetector.step               — наивный State (блок 3) внутри.
  */
object AnalysisPipeline:

  /** Итог анализа — всё, что нужно и для отчётов, и для отрисовки в окне. */
  case class Outcome(
      result: AnalysisResult,
      bands: Vector[BandSnapshot],
      summary: BandSummary,
      jsonContent: String,
      textContent: String
  )

  /** Развернуть лог наивного Writer в ZIO-консоль.
    * Это "мост" между блоком 2 (Writer) и блоком 4 (ZIO Console).
    */
  def emitLog(log: Vector[String]): URIO[Any, Unit] =
    ZIO.foreachDiscard(log)(line => Console.printLine(s"  [log] $line").orDie)

  /** Прогнать весь анализ. Требует Config в окружении (ZIO-аналог Reader).
    * Канал ошибки — Throwable: ошибки чтения WAV типизированы, а не летят
    * исключением сквозь unsafeRun, как было с наивным IO.
    */
  def run: ZIO[Config, Throwable, Outcome] =
    for
      cfg            <- ZIO.service[Config]
      _              <- Console.printLine("=== Анализатор аудио ===")
      _              <- Console.printLine(s"Файл: ${cfg.filePath}")

      wavAndLog      <- WavReader.read(cfg.filePath)
      (wav, wavLog)   = wavAndLog
      _              <- emitLog(wavLog)

      frames         <- ZIO.attempt(FFT.analyze(wav.samples, wav.header.sampleRate, cfg.fftSize))
      _              <- Console.printLine(s"FFT: ${frames.length} кадров")

      // Наивный Writer (блок 2): detectAll возвращает (лог, значение).
      beatWriter      = BeatDetector.detectAll(frames, cfg.beatThreshold, cfg.beatWindow)
      _              <- emitLog(beatWriter.log)
      (beats, bpm)    = beatWriter.value

      // ZIO-окружение (блок 1): analyzeAll достаёт Config через ZIO.service.
      bands          <- BandAnalyzer.analyzeAll(frames)
      summaryWriter   = BandAnalyzer.summary(bands)
      _              <- emitLog(summaryWriter.log)
      summary         = summaryWriter.value

      result          = AnalysisResult(wav.header, frames, beats, bpm)
      jsonContent    <- Report.buildJson(result, bands)
      textContent    <- Report.buildText(result, summary)
    yield Outcome(result, bands, summary, jsonContent, textContent)

  /** Анализ + сохранение отчётов на диск (используется в CLI). */
  def runAndSave: ZIO[Config, Throwable, Outcome] =
    for
      cfg     <- ZIO.service[Config]
      outcome <- run
      _       <- Report.save(outcome.jsonContent, cfg.outputJsonPath)
      _       <- Console.printLine(s"JSON-отчёт сохранён: ${cfg.outputJsonPath}")
      _       <- Report.save(outcome.textContent, cfg.outputTextPath)
      _       <- Console.printLine(s"Текстовый отчёт сохранён: ${cfg.outputTextPath}")
    yield outcome
