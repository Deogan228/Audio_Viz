package domain

/** Заголовок WAV-файла */
case class WavHeader(
    sampleRate: Int,
    channels: Int,
    bitsPerSample: Int,
    numSamples: Int
):
  def durationSeconds: Double = numSamples.toDouble / sampleRate

/** WAV-файл целиком: заголовок + сэмплы в [-1.0, 1.0] */
case class WavData(header: WavHeader, samples: Vector[Double])

/** Один кадр FFT — массив амплитуд по частотным бинам */
case class SpectrumFrame(
    bins: Vector[Double],
    sampleRate: Int,
    fftSize: Int
):
  def freqOf(i: Int): Double = i.toDouble * sampleRate / fftSize

  def dominantFreq: Double =
    if bins.isEmpty then 0.0
    else
      val maxIdx = bins.zipWithIndex.maxBy(_._1)._2
      freqOf(maxIdx)

  def energy: Double = bins.map(b => b * b).sum

/** Обнаруженный бит */
case class Beat(frameIndex: Int, timeSeconds: Double, energy: Double)

/** Результат анализа FFT + детекции битов */
case class AnalysisResult(
    header: WavHeader,
    frames: Vector[SpectrumFrame],
    beats: Vector[Beat],
    bpm: Double
)

/** Снимок энергий по диапазонам в момент времени */
case class BandSnapshot(
    timeSec: Double,
    bass: Double,
    mid: Double,
    high: Double,
    dominantFreq: Double
)

/** Сводка по диапазонам за весь трек */
case class BandSummary(
    avgBass: Double,
    avgMid: Double,
    avgHigh: Double,
    avgDominant: Double
)

/** Конфигурация анализатора — для Reader-монады */
case class Config(
    filePath: String,
    outputJsonPath: String = "report.json",
    outputTextPath: String = "report.txt",
    fftSize: Int = 1024,
    beatThreshold: Double = 1.3,
    beatWindow: Int = 43,
    bassRange: (Int, Int) = (30, 250),
    midRange:  (Int, Int) = (250, 4000),
    highRange: (Int, Int) = (4000, 20000)
)