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

  /** Состояние детектора: скользящее окно последних энергий */
  case class DetectorState(energyHistory: Vector[Double])

  object DetectorState:
    val empty: DetectorState = DetectorState(Vector.empty)

  /** Обрабатывает один кадр и обновляет состояние.
    * Возвращает Some(Beat) если кадр определён как бит.
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
      val isBeat =
        if history.length < windowSize then false
        else
          val avg = history.sum / history.length
          energy > avg * threshold

      // Обновляем окно: добавляем новую энергию, обрезаем по размеру
      val newHistory = (history :+ energy).takeRight(windowSize)
      val timeSec = frameIdx.toDouble * frame.fftSize / frame.sampleRate
      val beat = if isBeat then Some(Beat(frameIdx, timeSec, energy)) else None

      (DetectorState(newHistory), beat)
    }

  /** Прогоняет State по всем кадрам, собирает биты и считает BPM */
  def detectAll(
      frames: Vector[SpectrumFrame],
      threshold: Double,
      windowSize: Int
  ): Writer[Vector[String], (Vector[Beat], Double)] =
    // Последовательная композиция шагов через flatMap
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

  /** Считает BPM из интервалов между битами */
  private def calcBpm(beats: Vector[Beat]): Double =
    if beats.length < 2 then 0.0
    else
      val intervals = beats.zip(beats.tail).map((a, b) => b.timeSeconds - a.timeSeconds)
      val avgInterval = intervals.sum / intervals.length
      if avgInterval > 0 then 60.0 / avgInterval else 0.0