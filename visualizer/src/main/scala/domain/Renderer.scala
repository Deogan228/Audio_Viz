package domain

import zio.{ZIO, URIO, Ref}

// Логика подготовки кадра: по времени находим нужный снимок, проверяем бит, обновляем состояние
object Renderer:

  /** Кадр для отрисовки — что именно должен показать холст */
  case class Frame(
      snapshot: BandSnapshot,
      bpm: Double,
      mode: VisualMode,
      beatActive: Boolean,
      timeSec: Double
  )

  /** По времени найти индекс ближайшего снимка. Снимки идут равномерно, так что простое линейное приближение */
  def snapshotIdxAt(snapshots: Vector[BandSnapshot], timeSec: Double): Int =
    if snapshots.isEmpty then 0
    else
      val last = snapshots.length - 1
      val dt = if last > 0 then snapshots(last).timeSec / last else 1.0
      val raw = if dt > 0 then (timeSec / dt).toInt else 0
      math.min(last, math.max(0, raw))

  /** Есть ли бит рядом с данным моментом */
  def beatNearby(beatTimes: Vector[Double], timeSec: Double, windowSec: Double): Boolean =
    beatTimes.exists(bt => math.abs(bt - timeSec) < windowSec)

  /** Атомарно обновить состояние анимации (индекс кадра, счётчик вспышки бита) */
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

  /** Сменить режим визуализации на лету */
  def switchMode(stateRef: Ref[AnimationState], newMode: VisualMode): URIO[Any, Unit] =
    stateRef.update(_.copy(activeMode = newMode))

  /** Собрать готовый кадр из отчёта, состояния и времени */
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
