package domain

import monads.{IO, Reader, State}

/** Цикл анимации, синхронизированной с воспроизведением.
  *
  * Логика:
  *   1. Запускаем AudioPlayer
  *   2. В цикле запрашиваем у плеера реальную позицию воспроизведения
  *   3. По позиции находим текущий BandSnapshot
  *   4. Через State обновляем AnimationState
  *   5. Через Reader получаем строку рендера
  *   6. Печатаем на экран
  *
  * Императивный while-цикл инкапсулирован в IO.delay,
  * снаружи остаётся чистый функциональный интерфейс.
  * Это вынужденная мера: наивный IO без trampoline переполняет стек
  * на цепочке из тысяч flatMap.
  */
object Animation:

  /** Запуск визуализации */
  def run(report: Report): Reader[RenderConfig, IO[Unit]] =
    Reader.asks { cfg =>
      if report.bands.isEmpty then
        IO.println("В отчёте нет данных для визуализации")
      else
        for
          handle <- AudioPlayer.play(cfg.wavPath)
          _      <- Renderer.setup
          _      <- renderLoop(report, handle, cfg)
          _      <- AudioPlayer.stop(handle)
          _      <- Renderer.teardown
        yield ()
    }

  /** Главный цикл рендеринга.
    *
    * Состояние анимации передаётся через переменную animState
    * внутри IO.delay — это эквивалент State, развёрнутого в цикл
    * для избежания StackOverflow.
    */
  private def renderLoop(
      report: Report,
      handle: AudioPlayer.PlayerHandle,
      cfg: RenderConfig
  ): IO[Unit] = IO.delay {
    val Home = "\u001b[H"
    val beatTimes = report.beats.map(_.timeSec).toArray
    val snapshots = report.bands

    var animState = AnimationState.initial
    var lastRenderedIdx = -1

    while handle.isRunning && animState.currentSnapshotIdx < snapshots.length - 1 do
      val posSec = handle.positionSeconds
      val targetIdx = findSnapshotIdx(snapshots, posSec)

      if targetIdx > lastRenderedIdx then
        val isBeat = beatNearby(beatTimes, posSec, 0.05)
        // Обновляем состояние через State-монаду
        val (newState, _) = Renderer.tickAnimation(targetIdx, isBeat, cfg).run(animState)
        animState = newState

        // Рендерим кадр через Reader
        val rendered = Renderer
          .renderFrame(snapshots(targetIdx), report.analysis.bpm, animState)
          .run(cfg)

        print(Home)
        print(rendered)
        System.out.flush()
        lastRenderedIdx = targetIdx
      else
        Thread.sleep(5)
  }

  /** Бинарный поиск ближайшего по времени снимка */
  private def findSnapshotIdx(snapshots: Vector[BandSnapshot], timeSec: Double): Int =
    if snapshots.isEmpty then 0
    else
      val last = snapshots.length - 1
      // Линейная аппроксимация работает быстрее, т.к. снимки идут равномерно
      val dt = snapshots(last).timeSec / last
      math.min(last, math.max(0, (timeSec / dt).toInt))

  /** Был ли бит в окрестности времени? */
  private def beatNearby(beatTimes: Array[Double], timeSec: Double, windowSec: Double): Boolean =
    var i = 0
    var found = false
    while i < beatTimes.length && !found do
      if math.abs(beatTimes(i) - timeSec) < windowSec then found = true
      i += 1
    found
