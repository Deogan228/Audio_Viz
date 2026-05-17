package domain

import monads.IO

/** Визуализатор спектра в терминал через ANSI escape-коды.
  *
  * Возможности:
  *   - Спектр в виде цветных столбиков (бас/средние/высокие)
  *   - Логарифмическая шкала частот (как в реальных эквалайзерах)
  *   - Подписи частот по оси X
  *   - Текущее время, BPM, индикатор бита сверху
  *   - Синхронизация по реальной позиции аудио-плеера (нет дрейфа)
  */
object Visualizer:

  // ANSI-коды
  private val Clear      = "\u001b[2J"
  private val Home       = "\u001b[H"
  private val ClearLine  = "\u001b[K"
  private val Reset      = "\u001b[0m"
  private val Bold       = "\u001b[1m"
  private val HideCursor = "\u001b[?25l"
  private val ShowCursor = "\u001b[?25h"

  private val ColorBass   = "\u001b[31m"   // красный
  private val ColorMid    = "\u001b[33m"   // жёлтый
  private val ColorHigh   = "\u001b[36m"   // голубой
  private val ColorWhite  = "\u001b[97m"
  private val ColorBeatBg = "\u001b[41m"   // красный фон для индикатора бита

  /** Распределяет частотные бины по столбикам логарифмически.
    * Это правильнее линейного — низкие частоты получают больше места,
    * как в эквалайзере на музыкальной аппаратуре.
    */
  private def logBands(numBins: Int, width: Int, sampleRate: Int, fftSize: Int): Vector[(Int, Int)] =
    val minFreq = 30.0
    val maxFreq = sampleRate / 2.0
    val logMin = math.log(minFreq)
    val logMax = math.log(maxFreq)
    Vector.tabulate(width) { col =>
      val from = math.exp(logMin + (logMax - logMin) * col / width)
      val to   = math.exp(logMin + (logMax - logMin) * (col + 1) / width)
      val binFrom = ((from * fftSize) / sampleRate).toInt
      val binTo   = math.min(((to * fftSize) / sampleRate).toInt + 1, numBins)
      (binFrom, math.max(binTo, binFrom + 1))
    }

  /** Рендерит один кадр: верхняя строка статуса + спектр + ось частот */
  def renderFrame(
      frame: SpectrumFrame,
      height: Int,
      width: Int,
      timeSec: Double,
      bpm: Double,
      isBeat: Boolean
  ): String =
    val bands = logBands(frame.bins.length, width, frame.sampleRate, frame.fftSize)
    val cols = bands.map { (from, to) =>
      val slice = frame.bins.slice(from, math.min(to, frame.bins.length))
      if slice.isEmpty then 0.0 else slice.max
    }

    val maxVal = math.max(cols.max, 1e-9)
    val normalized = cols.map(v => math.sqrt(v / maxVal))

    val sb = new StringBuilder

    // === Верхняя строка статуса ===
    val timeStr = f"$timeSec%6.2fs"
    val bpmStr  = f"BPM: $bpm%5.1f"
    val beatStr = if isBeat then s"$ColorBeatBg$Bold BEAT $Reset" else "      "
    sb.append(s"$Bold$ColorWhite$timeStr   $bpmStr   $beatStr$Reset$ClearLine\n")
    sb.append(s"$Reset$ClearLine\n")  // пустая строка

    // === Сам спектр ===
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

    // === Ось частот снизу ===
    sb.append(renderFreqAxis(width, frame.sampleRate)).append(ClearLine).append('\n')
    sb.toString

  /** Рисует ось частот логарифмически: метки на разных частотах */
  private def renderFreqAxis(width: Int, sampleRate: Int): String =
    val minFreq = 30.0
    val maxFreq = sampleRate / 2.0
    val logMin = math.log(minFreq)
    val logMax = math.log(maxFreq)

    // Точки на оси: 50, 100, 200, 500, 1k, 2k, 5k, 10k Hz
    val markers = Vector(50, 100, 200, 500, 1000, 2000, 5000, 10000)
      .filter(f => f >= minFreq && f <= maxFreq)
      .map { f =>
        val col = ((math.log(f) - logMin) / (logMax - logMin) * width).toInt
        val label = if f >= 1000 then s"${f / 1000}k" else f.toString
        (col, label)
      }

    val line = Array.fill(width)(' ')
    for (col, label) <- markers do
      val start = math.max(0, math.min(width - label.length, col - label.length / 2))
      for (ch, i) <- label.zipWithIndex do
        if start + i < width then line(start + i) = ch

    s"$ColorWhite${line.mkString}$Reset"

  /** Анимация спектра с синхронизацией по реальной позиции аудио.
    *
    * Вместо подсчёта времени самостоятельно мы спрашиваем у плеера
    * "какая сейчас позиция воспроизведения?" и показываем кадр под неё.
    * Это устраняет дрейф между визуализацией и звуком.
    */
  def animateWithAudio(
      frames: Vector[SpectrumFrame],
      beats: Vector[Beat],
      bpm: Double,
      audioPath: String,
      height: Int,
      width: Int
  ): IO[Unit] =
    if frames.isEmpty then IO.pure(())
    else
      // Множество индексов кадров, в которых произошёл бит — для быстрой проверки
      val beatFrames = beats.map(_.frameIndex).toSet

      for
        handle <- AudioPlayer.play(audioPath)
        _      <- IO.delay { print(Clear); print(Home); print(HideCursor); System.out.flush() }
        _      <- renderLoop(frames, beatFrames, bpm, handle, height, width)
        _      <- AudioPlayer.stop(handle)
        _      <- IO.delay { print(ShowCursor); println(); println("Готово.") }
      yield ()

  /** Цикл рендеринга, синхронизированный по реальной позиции плеера.
    *
    * Императивный цикл инкапсулирован внутри IO.delay — снаружи интерфейс чистый.
    * Это вынужденная мера: наивный IO без trampoline переполняет стек
    * на цепочке из 10000+ flatMap.
    */
  private def renderLoop(
      frames: Vector[SpectrumFrame],
      beatFrames: Set[Int],
      bpm: Double,
      handle: AudioPlayer.PlayerHandle,
      height: Int,
      width: Int
  ): IO[Unit] = IO.delay {
    val fftSize    = frames.head.fftSize
    val sampleRate = frames.head.sampleRate
    val frameTimeSec = fftSize.toDouble / sampleRate

    // Подсветка бита держится несколько кадров чтобы успеть заметить
    val beatFlashFrames = 3

    var lastRenderedIdx = -1
    while handle.isRunning && lastRenderedIdx < frames.length - 1 do
      val posSec = handle.positionSeconds
      val targetIdx = math.min((posSec / frameTimeSec).toInt, frames.length - 1)

      if targetIdx > lastRenderedIdx then
        // Проверяем, был ли бит за последние beatFlashFrames кадров
        val isBeat = (math.max(0, targetIdx - beatFlashFrames) to targetIdx)
          .exists(beatFrames.contains)

        val rendered = renderFrame(
          frames(targetIdx), height, width, posSec, bpm, isBeat
        )
        print(Home)
        print(rendered)
        System.out.flush()  // принудительный сброс буфера — убирает задержку
        lastRenderedIdx = targetIdx
      else
        // Уже отрисовали этот кадр, ждём следующий
        Thread.sleep(5)
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