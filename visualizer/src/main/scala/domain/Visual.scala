package domain

/** Один бит, прочитанный из JSON-отчёта */
case class Beat(frameIndex: Int, timeSec: Double, energy: Double)

/** Снимок энергий по диапазонам в момент времени */
case class BandSnapshot(
    timeSec: Double,
    bass: Double,
    mid: Double,
    high: Double,
    dominantFreq: Double
)

/** Информация о исходном файле */
case class SourceInfo(
    path: String,
    sampleRate: Int,
    channels: Int,
    bitsPerSample: Int,
    durationSeconds: Double,
    numSamples: Long
)

/** Параметры анализа */
case class AnalysisInfo(
    fftSize: Int,
    framesCount: Int,
    frameDurationSec: Double,
    bpm: Double,
    beatsCount: Int
)

/** Полный отчёт от анализатора, прочитанный из JSON */
case class Report(
    source: SourceInfo,
    analysis: AnalysisInfo,
    beats: Vector[Beat],
    bands: Vector[BandSnapshot]
)

/** Режим визуализации */
enum VisualMode:
  case Spectrum       // столбики энергии по диапазонам
  case Spectrogram    // накопление по времени (опционально)
  case Bars3          // только три столбика: бас/средние/высокие

/** Состояние анимации — для State-монады */
case class AnimationState(
    currentSnapshotIdx: Int,
    activeMode: VisualMode,
    beatFlashFrames: Int,    // сколько кадров ещё показывать индикатор бита
    paused: Boolean
)

object AnimationState:
  val initial: AnimationState =
    AnimationState(0, VisualMode.Spectrum, 0, paused = false)

/** Конфигурация визуализатора — для Reader-монады */
case class RenderConfig(
    reportPath: String,
    wavPath: String,
    height: Int = 20,
    width: Int = 80,
    beatFlashDurationFrames: Int = 3,
    fps: Int = 30
)
