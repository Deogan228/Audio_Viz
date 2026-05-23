package domain

import ui.SpectrumCanvas

import zio.{ZIO, Ref, Scope, Duration}

/** Цикл анимации, синхронизированной с воспроизведением.
  *
  * В исходной версии это был императивный while-цикл с `var animState`
  * внутри IO.delay — вынужденная мера, потому что наивный IO без
  * trampoline переполнял стек на тысячах flatMap.
  *
  * Теперь цикл — это рекурсивный ZIO-эффект. ZIO безопасен по стеку
  * (имеет trampoline), поэтому рекурсия loop -> loop не переполняет стек
  * даже на длинном треке. Состояние анимации живёт в zio.Ref вместо var.
  *
  * Логика:
  *   1. Запускаем AudioPlayer (как ZIO-ресурс через Scope).
  *   2. На каждом тике спрашиваем у плеера реальную позицию.
  *   3. По позиции находим текущий снимок, обновляем Ref (State → Ref).
  *   4. Через Reader-окружение собираем кадр и отдаём его в Swing-canvas.
  *   5. Спим 1/fps и повторяем, пока трек не кончился.
  */
object Animation:

  /** Запуск визуализации. Требует RenderConfig в окружении (Reader → ZIO)
    * и Scope для управления жизнью аудио-ресурса.
    */
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

  /** Главный цикл рендеринга — рекурсивный ZIO-эффект.
    *
    * Условие остановки: плеер перестал играть ИЛИ дошли до последнего
    * снимка. Иначе — обновляем кадр и рекурсивно вызываем себя.
    */
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

    /** Один тик: позиция -> состояние -> кадр -> отрисовка. */
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
