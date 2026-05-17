package domain

import monads.{IO, Reader, Writer, Monoid}
import monads.Monoid.given

import java.io.{FileWriter, BufferedWriter}

/** Формирование и сохранение отчёта анализа.
  *
  * Поддерживается два формата:
  *   - JSON (для визуализатора и других программ)
  *   - человеко-читаемый текстовый отчёт (для пользователя)
  *
  * JSON делаем "ручками" без библиотек — для курсовой это плюс
  * (показываем что знаем формат), и не тянем лишнюю зависимость.
  */
object Report:

  /** Сформировать JSON-отчёт. Reader, т.к. зависит от Config. */
  def buildJson(
      result: AnalysisResult,
      bands: Vector[BandSnapshot]
  ): Reader[Config, String] =
    Reader.asks { cfg =>
      val sb = new StringBuilder
      sb.append("{\n")
      sb.append("  \"version\": 1,\n")

      // source
      sb.append("  \"source\": {\n")
      sb.append(s"""    "path": ${jsonString(cfg.filePath)},\n""")
      sb.append(s"""    "sampleRate": ${result.header.sampleRate},\n""")
      sb.append(s"""    "channels": ${result.header.channels},\n""")
      sb.append(s"""    "bitsPerSample": ${result.header.bitsPerSample},\n""")
      sb.append(s"""    "durationSeconds": ${num(result.header.durationSeconds)},\n""")
      sb.append(s"""    "numSamples": ${result.header.numSamples}\n""")
      sb.append("  },\n")

      // analysis
      val frameDur = cfg.fftSize.toDouble / result.header.sampleRate
      sb.append("  \"analysis\": {\n")
      sb.append(s"""    "fftSize": ${cfg.fftSize},\n""")
      sb.append(s"""    "framesCount": ${result.frames.length},\n""")
      sb.append(s"""    "frameDurationSec": ${num(frameDur)},\n""")
      sb.append(s"""    "bpm": ${num(result.bpm)},\n""")
      sb.append(s"""    "beatsCount": ${result.beats.length},\n""")
      sb.append(s"""    "beatThreshold": ${num(cfg.beatThreshold)},\n""")
      sb.append(s"""    "beatWindow": ${cfg.beatWindow}\n""")
      sb.append("  },\n")

      // beats
      sb.append("  \"beats\": [\n")
      val beatsJson = result.beats.map { b =>
        s"""    { "frameIndex": ${b.frameIndex}, "timeSec": ${num(b.timeSeconds)}, "energy": ${num(b.energy)} }"""
      }.mkString(",\n")
      sb.append(beatsJson)
      if result.beats.nonEmpty then sb.append("\n")
      sb.append("  ],\n")

      // bands
      sb.append("  \"bands\": [\n")
      val bandsJson = bands.map { b =>
        s"""    { "timeSec": ${num(b.timeSec)}, "bass": ${num(b.bass)}, "mid": ${num(b.mid)}, "high": ${num(b.high)}, "dominantFreq": ${num(b.dominantFreq)} }"""
      }.mkString(",\n")
      sb.append(bandsJson)
      if bands.nonEmpty then sb.append("\n")
      sb.append("  ]\n")

      sb.append("}\n")
      sb.toString
    }

  /** Сформировать человеко-читаемый текстовый отчёт */
  def buildText(
      result: AnalysisResult,
      bandSummary: BandSummary
  ): Reader[Config, String] =
    Reader.asks { cfg =>
      s"""=== Отчёт анализа аудио ===
         |
         |Файл:              ${cfg.filePath}
         |Длительность:      ${"%.2f".format(result.header.durationSeconds)} сек
         |Sample rate:       ${result.header.sampleRate} Hz
         |Каналов:           ${result.header.channels}
         |Битность:          ${result.header.bitsPerSample} бит
         |
         |=== Параметры анализа ===
         |Размер окна FFT:   ${cfg.fftSize}
         |Порог детекции:    ${cfg.beatThreshold}
         |Окно усреднения:   ${cfg.beatWindow} кадров
         |
         |=== Результаты ===
         |Кадров FFT:        ${result.frames.length}
         |Найдено битов:     ${result.beats.length}
         |BPM:               ${"%.1f".format(result.bpm)}
         |
         |=== Спектральный баланс ===
         |Бас (30–250 Hz):           ${"%.4f".format(bandSummary.avgBass)}
         |Средние (250–4000 Hz):     ${"%.4f".format(bandSummary.avgMid)}
         |Высокие (4000+ Hz):        ${"%.4f".format(bandSummary.avgHigh)}
         |Средняя домин. частота:    ${"%.1f".format(bandSummary.avgDominant)} Hz
         |
         |Жанровая характеристика:   ${classify(bandSummary)}
         |""".stripMargin
    }

  /** Сохранить строку в файл (IO-эффект) */
  def save(content: String, path: String): IO[Unit] = IO.delay {
    val writer = new BufferedWriter(new FileWriter(path))
    try writer.write(content)
    finally writer.close()
  }

  /** Простая жанровая характеристика по балансу энергий.
    * Не претендует на точность — это иллюстрация анализа.
    */
  private def classify(s: BandSummary): String =
    val total = s.avgBass + s.avgMid + s.avgHigh
    if total <= 0 then "недостаточно данных"
    else
      val bp = s.avgBass / total
      val mp = s.avgMid / total
      val hp = s.avgHigh / total
      if bp > 0.6 then "тяжёлый бас (электроника, хип-хоп)"
      else if hp > 0.4 then "много высоких (классика, акустика)"
      else if mp > 0.5 then "сбалансированный (вокал, поп, рок)"
      else "смешанный"

  /** Экранирование строки для JSON */
  private def jsonString(s: String): String =
    val escaped = s
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
    s"\"$escaped\""

  /** Форматирование числа с точкой (не запятой!) — иначе JSON невалиден */
  private def num(d: Double): String =
    if d.isNaN || d.isInfinite then "0"
    else "%.4f".formatLocal(java.util.Locale.US, d)