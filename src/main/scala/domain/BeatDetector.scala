package domain

import monads.{State, Writer, Monoid}
import monads.Monoid.given

/** Детектор битов по энергии звука.
  *
  * Идея: считаем энергию каждого кадра, ведём скользящее среднее последних N кадров.
  * Если энергия текущего кадра существенно выше среднего — это бит.
  *
  * Состояние (история энергий) держим в State-монаде вместо изменяемой коллекции.
  */
object BeatDetector:

  /** Минимальный интервал между битами в секундах.
    * 200мс соответствует максимальному BPM = 300, что покрывает все реальные жанры.
    */
  private val MinBeatIntervalSec = 0.2

  /** Состояние детектора:
    *  - energyHistory — скользящее окно энергий для адаптивного порога
    *  - lastBeatTime — время последнего обнаруженного бита (для refractory period)
    */
  case class DetectorState(
      energyHistory: Vector[Double],
      lastBeatTime: Double
  )

  object DetectorState:
    val empty: DetectorState = DetectorState(Vector.empty, -Double.MaxValue)

  /** Обрабатывает один кадр и обновляет состояние.
    * Бит фиксируется только если:
    *   1. Энергия выше скользящего среднего × порог
    *   2. С последнего бита прошло достаточно времени
    */
  def step(
      frameIdx: Int,
      frame: SpectrumFrame,
      threshold: Double,
      windowSize: Int
  ): State[DetectorState, Option[Beat]] =
    State { st =>
      val energy = frame.energy
      val history = st.energyHistory
      val timeSec = frameIdx.toDouble * frame.fftSize / frame.sampleRate

      val highEnergy =
        if history.length < windowSize then false
        else
          val avg = history.sum / history.length
          energy > avg * threshold

      val farEnough = (timeSec - st.lastBeatTime) >= MinBeatIntervalSec
      val isBeat = highEnergy && farEnough

      val newHistory = (history :+ energy).takeRight(windowSize)
      val newLastBeatTime = if isBeat then timeSec else st.lastBeatTime
      val beat = if isBeat then Some(Beat(frameIdx, timeSec, energy)) else None

      (DetectorState(newHistory, newLastBeatTime), beat)
    }

  /** Прогоняет State по всем кадрам, собирает биты и считает BPM */
  def detectAll(
      frames: Vector[SpectrumFrame],
      threshold: Double,
      windowSize: Int
  ): Writer[Vector[String], (Vector[Beat], Double)] =
    val program: State[DetectorState, Vector[Option[Beat]]] =
      frames.zipWithIndex.foldLeft(State.pure[DetectorState, Vector[Option[Beat]]](Vector.empty)) {
        case (acc, (frame, idx)) =>
          for
            beats <- acc
            b     <- step(idx, frame, threshold, windowSize)
          yield beats :+ b
      }

    val (_, results) = program.run(DetectorState.empty)
    val beats = results.flatten
    val bpm = calcBpm(beats)

    Writer(
      Vector(
        s"Обработано кадров: ${frames.length}",
        s"Найдено битов: ${beats.length}",
        s"BPM: ${"%.1f".format(bpm)}"
      ),
      (beats, bpm)
    )

  /** Считает BPM по медиане интервалов между битами.
    *
    * Почему медиана, а не среднее:
    *   - Среднее сильно искажается длинными паузами в начале/конце трека
    *   - Медиана устойчива к выбросам и даёт стабильный результат
    */
  private def calcBpm(beats: Vector[Beat]): Double =
    if beats.length < 2 then 0.0
    else
      val intervals = beats.zip(beats.tail)
        .map((a, b) => b.timeSeconds - a.timeSeconds)
        .filter(_ > 0)
        .sorted
      if intervals.isEmpty then 0.0
      else
        val medianInterval = intervals(intervals.length / 2)
        if medianInterval > 0 then 60.0 / medianInterval else 0.0