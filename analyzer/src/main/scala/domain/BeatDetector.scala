package domain

import monads.{State, Writer, Monoid}
import monads.Monoid.given
import scala.annotation.tailrec

object BeatDetector:
  private val MinBeatIntervalSec = 0.2
  private val MinMusicalBpm = 70.0
  private val MaxMusicalBpm = 180.0

  case class DetectorState(
      energyHistory: Vector[Double],
      lastBeatTime: Double
  )

  object DetectorState:
    val empty: DetectorState = DetectorState(Vector.empty, -Double.MaxValue)

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
    val rawBpm = calcRawBpm(beats)
    val bpm = normalizeBpm(rawBpm)

    Writer(
      Vector(
        s"Обработано кадров: ${frames.length}",
        s"Найдено битов: ${beats.length}",
        s"BPM (сырой): ${"%.1f".format(rawBpm)}",
        s"BPM (нормализованный): ${"%.1f".format(bpm)}"
      ),
      (beats, bpm)
    )

  private def calcRawBpm(beats: Vector[Beat]): Double =
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

  private def normalizeBpm(raw: Double): Double =
    if raw <= 0 then 0.0
    else
      @tailrec
      def fix(bpm: Double): Double =
        if bpm > MaxMusicalBpm then fix(bpm / 2.0)
        else if bpm < MinMusicalBpm then fix(bpm * 2.0)
        else bpm
      fix(raw)