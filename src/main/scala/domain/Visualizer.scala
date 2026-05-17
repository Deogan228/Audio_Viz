package domain

import monads.IO

/** Визуализатор спектра в терминал через ANSI escape-коды */
object Visualizer:

  // ANSI-коды
  private val Clear      = "\u001b[2J"
  private val Home       = "\u001b[H"
  private val ClearLine  = "\u001b[K"
  private val Reset      = "\u001b[0m"
  private val HideCursor = "\u001b[?25l"
  private val ShowCursor = "\u001b[?25h"

  private val ColorBass = "\u001b[31m"
  private val ColorMid  = "\u001b[33m"
  private val ColorHigh = "\u001b[36m"

  /** Рендерит один кадр спектра */
  def renderFrame(frame: SpectrumFrame, height: Int, width: Int): String =
    val binsPerCol = math.max(1, frame.bins.length / width)
    val cols = (0 until width).map { col =>
      val from = col * binsPerCol
      val to   = math.min(from + binsPerCol, frame.bins.length)
      if from >= frame.bins.length then 0.0
      else frame.bins.slice(from, to).max
    }

    val maxVal = if cols.isEmpty then 1.0 else math.max(cols.max, 1e-9)
    val normalized = cols.map(v => math.sqrt(v / maxVal))

    val sb = new StringBuilder
    for row <- 0 until height do
      val rowHeight = (height - row).toDouble / height
      for (v, colIdx) <- normalized.zipWithIndex do
        val color =
          if colIdx < width / 4 then ColorBass
          else if colIdx < width * 3 / 4 then ColorMid
          else ColorHigh
        val ch = if v >= rowHeight then '█' else ' '
        sb.append(color).append(ch)
      sb.append(Reset).append(ClearLine).append('\n')
    sb.toString

  /** Анимация спектра с синхронизацией под воспроизведение аудио.
    *
    * Кадры FFT идут с фиксированной скоростью: один кадр на каждые
    * fftSize сэмплов. При sampleRate=44100 и fftSize=1024 это ~43 кадра/сек.
    *
    * Для синхронизации со звуком мы:
    *   1. Запоминаем момент старта (System.nanoTime).
    *   2. Для каждого кадра вычисляем его "идеальное" время.
    *   3. Если опаздываем — пропускаем кадр, если опережаем — ждём.
    */
  def animateWithAudio(
      frames: Vector[SpectrumFrame],
      audioPath: String,
      height: Int,
      width: Int
  ): IO[Unit] =
    if frames.isEmpty then IO.pure(())
    else
      val fftSize    = frames.head.fftSize
      val sampleRate = frames.head.sampleRate
      val frameTimeNs = (fftSize.toLong * 1_000_000_000L) / sampleRate

      for
        handle <- AudioPlayer.play(audioPath)
        _      <- IO.delay { print(Clear); print(Home); print(HideCursor) }
        _      <- renderLoop(frames, frameTimeNs, height, width)
        _      <- AudioPlayer.stop(handle)
        _      <- IO.delay { print(ShowCursor); println(); println("Готово.") }
      yield ()

  /** Цикл рендеринга, синхронизированный с реальным временем */
  private def renderLoop(
      frames: Vector[SpectrumFrame],
      frameTimeNs: Long,
      height: Int,
      width: Int
  ): IO[Unit] = IO.delay {
    val startTime = System.nanoTime()
    var i = 0
    while i < frames.length do
      val targetTime = startTime + i * frameTimeNs
      val now = System.nanoTime()
      val sleepNs = targetTime - now

      if sleepNs > 0 then
        // Опережаем: ждём
        Thread.sleep(sleepNs / 1_000_000, (sleepNs % 1_000_000).toInt)
        print(Home)
        print(renderFrame(frames(i), height, width))
        i += 1
      else if sleepNs > -frameTimeNs then
        // Чуть опаздываем: рендерим без задержки
        print(Home)
        print(renderFrame(frames(i), height, width))
        i += 1
      else
        // Сильно опаздываем: пропускаем кадр без рендеринга
        i += 1
  }

  /** Краткая текстовая сводка по результату анализа */
  def summary(result: AnalysisResult): String =
    s"""
       |=== Результат анализа ===
       |Длительность:    ${"%.2f".format(result.header.durationSeconds)} сек
       |Sample rate:     ${result.header.sampleRate} Hz
       |Каналов:         ${result.header.channels}
       |Кадров FFT:      ${result.frames.length}
       |Найдено битов:   ${result.beats.length}
       |BPM:             ${"%.1f".format(result.bpm)}
       |""".stripMargin