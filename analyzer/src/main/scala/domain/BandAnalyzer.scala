package domain

import monads.{Writer, Monoid}
import monads.Monoid.given
import zio.{ZIO, URIO}

/** Анализ FFT-кадров по частотным диапазонам.
  *
  * Для каждого кадра считаем энергию в басе (30–250 Hz), средних
  * (250–4000 Hz), высоких (4000+ Hz) и доминирующую частоту.
  *
  * Блок 1 (Reader → ZIO): границы диапазонов берутся из Config. В исходной
  * версии это был Reader[Config, A]. Теперь — ZIO-окружение: тип
  * URIO[Config, A] = ZIO[Config, Nothing, A]. Конфиг достаётся через
  * ZIO.service[Config], а поставляется через ZLayer (см. Config.layer).
  * Это и есть промышленный аналог Reader-монады.
  *
  * Блок 2 (Writer): summary возвращает наивный Writer — второе из "пары
  * мест", где по ТЗ Writer остаётся буквально.
  */
object BandAnalyzer:

  /** Анализ одного кадра. URIO[Config, _] — зависит от конфига в окружении. */
  def analyzeFrame(frame: SpectrumFrame, timeSec: Double): URIO[Config, BandSnapshot] =
    ZIO.serviceWith[Config] { cfg =>
      val bass = energyInRange(frame, cfg.bassRange._1, cfg.bassRange._2)
      val mid  = energyInRange(frame, cfg.midRange._1, cfg.midRange._2)
      val high = energyInRange(frame, cfg.highRange._1, cfg.highRange._2)
      BandSnapshot(timeSec, bass, mid, high, frame.dominantFreq)
    }

  /** Анализ всех кадров. ZIO достаёт Config из окружения один раз
    * и прогоняет чистое вычисление по всем кадрам.
    */
  def analyzeAll(frames: Vector[SpectrumFrame]): URIO[Config, Vector[BandSnapshot]] =
    ZIO.serviceWith[Config] { cfg =>
      frames.zipWithIndex.map { (frame, idx) =>
        val timeSec = idx.toDouble * frame.fftSize / frame.sampleRate
        val bass = energyInRange(frame, cfg.bassRange._1, cfg.bassRange._2)
        val mid  = energyInRange(frame, cfg.midRange._1, cfg.midRange._2)
        val high = energyInRange(frame, cfg.highRange._1, cfg.highRange._2)
        BandSnapshot(timeSec, bass, mid, high, frame.dominantFreq)
      }
    }

  /** Энергия в заданном диапазоне частот (Hz). Чистая функция. */
  private def energyInRange(frame: SpectrumFrame, fromHz: Int, toHz: Int): Double =
    val fromBin = ((fromHz.toDouble * frame.fftSize) / frame.sampleRate).toInt
    val toBin   = math.min(((toHz.toDouble * frame.fftSize) / frame.sampleRate).toInt + 1,
                            frame.bins.length)
    if fromBin >= frame.bins.length then 0.0
    else frame.bins.slice(fromBin, toBin).map(b => b * b).sum

  /** Сводка по всему треку с диагностическим логом в наивном Writer. */
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
