package domain

import monads.{Reader, Writer, Monoid}
import monads.Monoid.given

/** Анализ FFT-кадров по частотным диапазонам.
  *
  * Для каждого кадра считаем:
  *  - энергию в басе (30–250 Hz)
  *  - энергию в средних (250–4000 Hz)
  *  - энергию в высоких (4000+ Hz)
  *  - доминирующую частоту
  *
  * Используется Reader[Config, ...] — границы диапазонов берутся из конфига.
  */
object BandAnalyzer:

  /** Анализ одного кадра. Возвращает Reader, т.к. границы — из конфига. */
  def analyzeFrame(frame: SpectrumFrame): Reader[Config, BandSnapshot] =
    Reader.asks { cfg =>
      val timeSec = frame.fftSize.toDouble * 0 / frame.sampleRate
      val bass    = energyInRange(frame, cfg.bassRange._1, cfg.bassRange._2)
      val mid     = energyInRange(frame, cfg.midRange._1, cfg.midRange._2)
      val high    = energyInRange(frame, cfg.highRange._1, cfg.highRange._2)
      val dominant = frame.dominantFreq
      BandSnapshot(timeSec, bass, mid, high, dominant)
    }

  /** Анализ всех кадров. Reader пробрасывает Config во все вычисления. */
  def analyzeAll(frames: Vector[SpectrumFrame]): Reader[Config, Vector[BandSnapshot]] =
    Reader.asks { cfg =>
      frames.zipWithIndex.map { (frame, idx) =>
        val timeSec = idx.toDouble * frame.fftSize / frame.sampleRate
        val bass    = energyInRange(frame, cfg.bassRange._1, cfg.bassRange._2)
        val mid     = energyInRange(frame, cfg.midRange._1, cfg.midRange._2)
        val high    = energyInRange(frame, cfg.highRange._1, cfg.highRange._2)
        BandSnapshot(timeSec, bass, mid, high, frame.dominantFreq)
      }
    }

  /** Энергия в заданном диапазоне частот (Hz) */
  private def energyInRange(frame: SpectrumFrame, fromHz: Int, toHz: Int): Double =
    val fromBin = ((fromHz.toDouble * frame.fftSize) / frame.sampleRate).toInt
    val toBin   = math.min(((toHz.toDouble * frame.fftSize) / frame.sampleRate).toInt + 1,
                            frame.bins.length)
    if fromBin >= frame.bins.length then 0.0
    else frame.bins.slice(fromBin, toBin).map(b => b * b).sum

  /** Сводка по всему треку с диагностическим логом */
  def summary(snapshots: Vector[BandSnapshot]): Writer[Vector[String], BandSummary] =
    if snapshots.isEmpty then
      Writer(Vector("Нет данных для анализа диапазонов"), BandSummary(0, 0, 0, 0))
    else
      val avgBass = snapshots.map(_.bass).sum / snapshots.length
      val avgMid  = snapshots.map(_.mid).sum / snapshots.length
      val avgHigh = snapshots.map(_.high).sum / snapshots.length
      val avgDominant = snapshots.map(_.dominantFreq).sum / snapshots.length

      Writer(
        Vector(
          s"Средняя энергия баса:    ${"%.4f".format(avgBass)}",
          s"Средняя энергия средних: ${"%.4f".format(avgMid)}",
          s"Средняя энергия высоких: ${"%.4f".format(avgHigh)}",
          s"Средняя домин. частота:  ${"%.1f".format(avgDominant)} Hz"
        ),
        BandSummary(avgBass, avgMid, avgHigh, avgDominant)
      )