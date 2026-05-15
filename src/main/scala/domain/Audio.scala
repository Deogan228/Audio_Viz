package domain

/** Заголовок WAV-файла */
case class WavHeader(
    sampleRate: Int,      // обычно 44100
    channels: Int,        // 1 = моно, 2 = стерео
    bitsPerSample: Int,   // обычно 16
    numSamples: Int       // всего сэмплов на канал
):
  def durationSeconds: Double = numSamples.toDouble / sampleRate

/** WAV-файл целиком: заголовок + сэмплы в [-1.0, 1.0] (моно, склеенное из каналов) */
case class WavData(header: WavHeader, samples: Vector[Double])

/** Один кадр FFT — массив амплитуд по частотным бинам */
case class SpectrumFrame(
    bins: Vector[Double],
    sampleRate: Int,
    fftSize: Int
):
  /** Частота бина i в Hz */
  def freqOf(i: Int): Double = i.toDouble * sampleRate / fftSize

  /** Доминирующая частота кадра */
  def dominantFreq: Double =
    if bins.isEmpty then 0.0
    else
      val maxIdx = bins.zipWithIndex.maxBy(_._1)._2
      freqOf(maxIdx)

  /** Суммарная энергия кадра */
  def energy: Double = bins.map(b => b * b).sum

/** Обнаруженный бит — индекс кадра и его энергия */
case class Beat(frameIndex: Int, timeSeconds: Double, energy: Double)

/** Результат анализа файла целиком */
case class AnalysisResult(
    header: WavHeader,
    frames: Vector[SpectrumFrame],
    beats: Vector[Beat],
    bpm: Double
)

/** Конфигурация — для Reader-монады */
case class Config(
    filePath: String,
    fftSize: Int = 1024,          // размер окна FFT (степень двойки)
    beatThreshold: Double = 1.3,  // во сколько раз энергия выше среднего = бит
    beatWindow: Int = 43          // окно для скользящего среднего (~1 сек при 44.1кГц/1024)
)