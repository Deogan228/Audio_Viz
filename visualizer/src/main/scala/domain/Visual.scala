package domain

import zio.{ZLayer, ULayer}

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

/** Информация об исходном файле */
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
  case Spectrogram    // накопление по времени
  case Bars3          // три крупных столбика: бас/средние/высокие

/** Состояние анимации.
  *
  * В исходной версии это состояние держалось в наивной State-монаде,
  * а реально менялось через `var` внутри while-цикла. Теперь это
  * иммутабельный снимок, который хранится и обновляется через zio.Ref —
  * это ZIO-замена State (блок 3 ТЗ).
  */
case class AnimationState(
    currentSnapshotIdx: Int,
    activeMode: VisualMode,
    beatFlashFrames: Int,    // сколько кадров ещё показывать индикатор бита
    paused: Boolean
)

object AnimationState:
  val initial: AnimationState =
    AnimationState(0, VisualMode.Spectrum, 0, paused = false)

/** Конфигурация визуализатора.
  *
  * В исходной версии пробрасывалась через наивный Reader[RenderConfig, A].
  * Теперь это ZIO-окружение: рендер-функции имеют тип
  * ZIO[RenderConfig, E, A], а конфиг поставляется через ZLayer.
  */
case class RenderConfig(
    reportPath: String,
    wavPath: String,
    height: Int = 20,
    width: Int = 80,
    beatFlashDurationFrames: Int = 3,
    fps: Int = 30,
    mode: VisualMode = VisualMode.Spectrum
)

object RenderConfig:
  /** ZLayer, поставляющий RenderConfig в окружение ZIO — замена Reader. */
  def layer(cfg: RenderConfig): ULayer[RenderConfig] = ZLayer.succeed(cfg)
