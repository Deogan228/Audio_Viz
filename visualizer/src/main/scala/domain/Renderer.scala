package domain

import zio.{ZIO, URIO, Ref}

/** Логика кадра визуализации.
  *
  * В исходной (терминальной) версии Renderer строил ANSI-строку. Теперь
  * интерфейс — графическое Swing-окно, поэтому собственно рисование
  * вынесено в ui/SpectrumCanvas. Здесь остаётся чистая логика:
  *
  *   - выбор данных кадра по времени воспроизведения;
  *   - обновление AnimationState (блок 3) — но не через наивный State,
  *     а через zio.Ref: tickAnimation атомарно меняет состояние в Ref.
  *
  * Reader заменён на ZIO-окружение RenderConfig (блок 1).
  */
object Renderer:

  /** Готовый к отрисовке кадр — то, что увидит UI. */
  case class Frame(
      snapshot: BandSnapshot,
      bpm: Double,
      mode: VisualMode,
      beatActive: Boolean,
      timeSec: Double
  )

  /** Найти индекс снимка по времени воспроизведения.
    * Снимки идут равномерно, поэтому линейная аппроксимация точна и быстра.
    */
  def snapshotIdxAt(snapshots: Vector[BandSnapshot], timeSec: Double): Int =
    if snapshots.isEmpty then 0
    else
      val last = snapshots.length - 1
      val dt = if last > 0 then snapshots(last).timeSec / last else 1.0
      val raw = if dt > 0 then (timeSec / dt).toInt else 0
      math.min(last, math.max(0, raw))

  /** Есть ли бит рядом с данным моментом времени. */
  def beatNearby(beatTimes: Vector[Double], timeSec: Double, windowSec: Double): Boolean =
    beatTimes.exists(bt => math.abs(bt - timeSec) < windowSec)

  /** Обновить состояние анимации в Ref — ZIO-замена State-монады (блок 3).
    *
    * Ref.update атомарно применяет чистую функцию перехода. Это ровно то,
    * что делала State, но потокобезопасно и без ручного протаскивания
    * состояния через while-цикл.
    */
  def tickAnimation(
      stateRef: Ref[AnimationState],
      newSnapshotIdx: Int,
      isBeat: Boolean
  ): URIO[RenderConfig, AnimationState] =
    ZIO.serviceWithZIO[RenderConfig] { cfg =>
      stateRef.updateAndGet { st =>
        val newFlash =
          if isBeat then cfg.beatFlashDurationFrames
          else math.max(0, st.beatFlashFrames - 1)
        st.copy(
          currentSnapshotIdx = newSnapshotIdx,
          beatFlashFrames = newFlash
        )
      }
    }

  /** Переключить режим визуализации — тоже атомарно через Ref. */
  def switchMode(stateRef: Ref[AnimationState], newMode: VisualMode): URIO[Any, Unit] =
    stateRef.update(_.copy(activeMode = newMode))

  /** Собрать кадр для отрисовки. Зависит от RenderConfig (Reader → ZIO). */
  def buildFrame(
      report: Report,
      anim: AnimationState,
      timeSec: Double
  ): URIO[RenderConfig, Frame] =
    ZIO.succeed {
      val idx = anim.currentSnapshotIdx
      val snap =
        if report.bands.isEmpty then BandSnapshot(timeSec, 0, 0, 0, 0)
        else report.bands(math.min(idx, report.bands.length - 1))
      Frame(
        snapshot   = snap,
        bpm        = report.analysis.bpm,
        mode       = anim.activeMode,
        beatActive = anim.beatFlashFrames > 0,
        timeSec    = timeSec
      )
    }
