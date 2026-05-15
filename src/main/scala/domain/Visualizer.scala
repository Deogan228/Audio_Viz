package domain

import monads.IO

/** Визуализатор спектра в терминал через ANSI escape-коды */
object Visualizer:

  // ANSI-коды
  private val Clear = "\u001b[2J"
  private val Home  = "\u001b[H"
  private val Reset = "\u001b[0m"

  // Цвета для разных частотных диапазонов
  private val ColorBass = "\u001b[31m"   // красный — басы
  private val ColorMid  = "\u001b[33m"   // жёлтый — средние
  private val ColorHigh = "\u001b[36m"   // голубой — высокие

  private val BarChars = " ▁▂▃▄▅▆▇█"

  /** Рендерит один кадр спектра как набор столбиков */
  def renderFrame(frame: SpectrumFrame, height: Int, width: Int): String =
    // Делим бины на группы — по одной на столбик
    val binsPerCol = math.max(1, frame.bins.length / width)
    val cols = (0 until width).map { col =>
      val from = col * binsPerCol
      val to   = math.min(from + binsPerCol, frame.bins.length)
      if from >= frame.bins.length then 0.0
      else frame.bins.slice(from, to).max
    }

    // Нормализуем
    val maxVal = if cols.isEmpty then 1.0 else math.max(cols.max, 1e-9)
    val normalized = cols.map(v => v / maxVal)

    // Рендерим
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
      sb.append(Reset).append('\n')
    sb.toString

  /** Показывает спектр всех кадров с задержкой между ними (анимация) */
  def animate(frames: Vector[SpectrumFrame], fps: Int, height: Int, width: Int): IO[Unit] =
    val frameDelayMs = 1000 / fps
    val program = frames.foldLeft(IO.pure(())) { (acc, frame) =>
      for
        _ <- acc
        _ <- IO.delay { print(Clear); print(Home) }
        _ <- IO.delay(println(renderFrame(frame, height, width)))
        _ <- IO.delay(Thread.sleep(frameDelayMs))
      yield ()
    }
    program

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