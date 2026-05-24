package domain

import zio.{ZLayer, ULayer}

/** Один бит, прочитанный из JSON-отчёта */
case class Beat(frameIndex: Int, timeSec: Double, energy: Double)

// Снимок громкости по частотам в конкретный момент времени
case class BandSnapshot(
    timeSec: Double,
    bass: Double,
    mid: Double,
    high: Double,
    dominantFreq: Double
)

// Информация об исходном аудиофайле
case class SourceInfo(
    path: String,
    sampleRate: Int,
    channels: Int,
    bitsPerSample: Int,
    durationSeconds: Double,
    numSamples: Long
)

// Параметры, с которыми делался анализ
case class AnalysisInfo(
    fftSize: Int,
    framesCount: Int,
    frameDurationSec: Double,
    bpm: Double,
    beatsCount: Int
)

// Весь отчёт, который мы прочитали из JSON
case class Report(
    source: SourceInfo,
    analysis: AnalysisInfo,
    beats: Vector[Beat],
    bands: Vector[BandSnapshot]
)

// Как именно будем показывать звук
enum VisualMode:
  case Spectrum       // столбики по частотам
  case Spectrogram    // накопление по времени
  case Bars3          // три крупные полосы: бас/середина/верха

// Состояние анимации: где мы сейчас, что показываем, мигает ли бит
// Хранится в ZIO Ref, чтобы менять без мутаций
case class AnimationState(
    currentSnapshotIdx: Int,
    activeMode: VisualMode,
    beatFlashFrames: Int,    // сколько кадров ещё показывать индикатор бита
    paused: Boolean
)

object AnimationState:
  val initial: AnimationState =
    AnimationState(0, VisualMode.Spectrum, 0, paused = false)

// Настройки визуализации: пути к файлам, размеры, FPS, режим
// Подаётся в ZIO-окружение, чтобы не таскать параметры явно
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
  // ZLayer, который кладёт конфиг в окружение
  def layer(cfg: RenderConfig): ULayer[RenderConfig] = ZLayer.succeed(cfg)
