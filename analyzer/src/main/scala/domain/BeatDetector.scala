package domain

import monads.{State, Writer, Monoid}
import monads.Monoid.given
import scala.annotation.tailrec

/** Детектор битов по энергии звука.
  *
  * Идея: считаем энергию каждого кадра, ведём скользящее среднее последних
  * N кадров. Если энергия текущего кадра существенно выше среднего — это бит.
  *
  * --- ЗДЕСЬ НАМЕРЕННО ИСПОЛЬЗУЮТСЯ СОБСТВЕННЫЕ МОНАДЫ ---
  *
  * Блок 3 (State): состояние детектора (история энергий, время последнего
  * бита) держим в наивной State-монаде из блока 0.
  *
  * Блок 2 (Writer): результат detectAll возвращается в наивном Writer —
  * это одно из "пары мест", где по ТЗ Writer остаётся как есть. ZIO-сценарий
  * в app/ просто разворачивает накопленный лог в консоль.
  *
  * Само вычисление чистое и тотальное, поэтому ZIO здесь не нужен:
  * эффектов (файлы, консоль) тут нет.
  */
object BeatDetector:

  /** Минимальный интервал между битами в секундах.
    * 200мс соответствует максимальному BPM = 300.
    */
  private val MinBeatIntervalSec = 0.2

  /** Состояние детектора (для State-монолады):
    *  - energyHistory — скользящее окно энергий для адаптивного порога
    *  - lastBeatTime — время последнего бита (refractory period)
    */
  case class DetectorState(
      energyHistory: Vector[Double],
      lastBeatTime: Double
  )

  object DetectorState:
    val empty: DetectorState = DetectorState(Vector.empty, -Double.MaxValue)

  /** Обрабатывает один кадр и обновляет состояние.
    * Возвращает State[DetectorState, Option[Beat]] — чистое описание
    * перехода состояния.
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

  /** Прогоняет State по всем кадрам через хвостовую рекурсию.
    *
    * Мы НЕ строим большую State-программу через foldLeft + for — наша
    * наивная State без trampoline переполнила бы стек на 10к+ кадрах.
    * Вместо этого вызываем step.run(state) пошагово. Семантически это
    * эквивалентно for-comprehension над State.
    *
    * Результат заворачивается в наивный Writer (блок 2).
    */
  def detectAll(
      frames: Vector[SpectrumFrame],
      threshold: Double,
      windowSize: Int
  ): Writer[Vector[String], (Vector[Beat], Double)] =

    @tailrec
    def loop(
        idx: Int,
        state: DetectorState,
        acc: Vector[Beat]
    ): Vector[Beat] =
      if idx >= frames.length then acc
      else
        val (newState, maybeBeat) =
          step(idx, frames(idx), threshold, windowSize).run(state)
        val newAcc = maybeBeat match
          case Some(b) => acc :+ b
          case None    => acc
        loop(idx + 1, newState, newAcc)

    val beats = loop(0, DetectorState.empty, Vector.empty)
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
    * Медиана устойчива к выбросам — лучше среднего для нашей задачи.
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
