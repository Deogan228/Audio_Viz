package domain

import monads.IO

/** Визуализатор спектра в терминал через ANSI escape-коды */
object Visualizer:

  // ANSI-коды
  private val Clear     = "\u001b[2J"   // очистить весь экран
  private val Home      = "\u001b[H"    // курсор в (1,1)
  private val ClearLine = "\u001b[K"    // очистить строку до конца
  private val Reset     = "\u001b[0m"
  private val HideCursor = "\u001b[?25l"
  private val ShowCursor = "\u001b[?25h"

  // Цвета для разных частотных диапазонов
  private val ColorBass = "\u001b[31m"   // красный — басы
  private val ColorMid  = "\u001b[33m"   // жёлтый — средние
  private val ColorHigh = "\u001b[36m"   // голубой — высокие

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

    // Нормализуем (логарифмически — звук лучше воспринимается так)
    val maxVal = if cols.isEmpty then 1.0 else math.max(cols.max, 1e-9)
    val normalized = cols.map(v => math.sqrt(v / maxVal))

    // Рендерим. Идём по строкам сверху вниз
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

  /** Показывает спектр всех кадров с задержкой между ними (анимация).
    *
    * Ключевая фишка: перед каждым кадром возвращаем курсор в (1,1)
    * через "\u001b[H", и каждая строка чистится через "\u001b[K".
    * Так кадры рисуются поверх друг друга, не съезжая вниз.
    */
  def animate(frames: Vector[SpectrumFrame], fps: Int, height: Int, width: Int): IO[Unit] =
    val frameDelayMs = 1000 / fps
    val setup = IO.delay { print(Clear); print(Home); print(HideCursor) }
    val cleanup = IO.delay { print(ShowCursor); println(); println("Готово.") }

    val frameProgram = frames.foldLeft(IO.pure(())) { (acc, frame) =>
      for
        _ <- acc
        _ <- IO.delay { print(Home); print(renderFrame(frame, height, width)) }
        _ <- IO.delay(Thread.sleep(frameDelayMs))
      yield ()
    }

    for
      _ <- setup
      _ <- frameProgram
      _ <- cleanup
    yield ()

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