package domain

import ui.SpectrumCanvas

import zio.{ZIO, Ref, Scope, Duration}

// Цикл анимации: получаем текущую позицию в треке, обновляем кадр и отправляем на холст.
// Звук играет параллельно, а мы с ним синхронизируемся.
object Animation:

  /** Запустить визуализацию. Требует настройки (RenderConfig) и область жизни аудио-ресурса (Scope). */
  def run(report: Report, canvas: SpectrumCanvas): ZIO[RenderConfig & Scope, Throwable, Unit] =
    if report.bands.isEmpty then
      zio.Console.printLine("В отчёте нет данных для визуализации").orDie
    else
      for
        cfg      <- ZIO.service[RenderConfig]
        handle   <- AudioPlayer.play(cfg.wavPath)
        stateRef <- Ref.make(AnimationState.initial.copy(activeMode = cfg.mode))
        beatTimes = report.beats.map(_.timeSec)
        _        <- loop(report, handle, stateRef, beatTimes, canvas, cfg)
      yield ()

  /** Главный цикл. Пока играет музыка и есть кадры — обновляем экран. */
  private def loop(
      report: Report,
      handle: AudioPlayer.PlayerHandle,
      stateRef: Ref[AnimationState],
      beatTimes: Vector[Double],
      canvas: SpectrumCanvas,
      cfg: RenderConfig
  ): ZIO[RenderConfig, Throwable, Unit] =
    val frameDelay = Duration.fromMillis(math.max(1, 1000 / cfg.fps).toLong)
    val lastIdx = report.bands.length - 1

    // Один тик: позиция плеера -> индекс кадра -> обновление состояния -> отрисовка.
    val tick: ZIO[RenderConfig, Throwable, Boolean] =
      for
        running <- handle.isRunning
        posSec  <- handle.positionSeconds
        idx      = Renderer.snapshotIdxAt(report.bands, posSec)
        isBeat   = Renderer.beatNearby(beatTimes, posSec, 0.05)
        anim    <- Renderer.tickAnimation(stateRef, idx, isBeat)
        frame   <- Renderer.buildFrame(report, anim, posSec)
        _       <- ZIO.attempt(canvas.showFrame(frame))
      yield running && anim.currentSnapshotIdx < lastIdx

    tick.flatMap { keepGoing =>
      if keepGoing then
        ZIO.sleep(frameDelay) *> loop(report, handle, stateRef, beatTimes, canvas, cfg)
      else
        ZIO.unit
    }
