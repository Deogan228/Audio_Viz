package domain

import monads.{Writer, Monoid}
import monads.Monoid.given
import zio.{ZIO, Task}

import scala.io.Source

// Чтение JSON-отчёта анализатора. Возвращает структуру с данными и диагностический лог
object ReportReader:

  /** Прочитать файл, распарсить и собрать лог загрузки */
  def read(path: String): Task[Writer[Vector[String], Report]] =
    for
      raw    <- readFile(path)
      report <- ZIO.attempt(jsonToReport(SimpleJson.parse(raw)))
    yield
      val log = Vector(
        s"Загружен отчёт: $path",
        s"Источник: ${report.source.path}",
        s"Длительность: ${"%.2f".format(report.source.durationSeconds)} сек",
        s"FFT-кадров: ${report.analysis.framesCount}",
        s"Битов в отчёте: ${report.beats.length}",
        s"BPM: ${"%.1f".format(report.analysis.bpm)}",
        s"Точек анализа диапазонов: ${report.bands.length}"
      )
      Writer(log, report)

  /** Открываем файл, читаем всё содержимое, автоматически закрываем */
  private def readFile(path: String): Task[String] =
    ZIO.acquireReleaseWith(ZIO.attempt(Source.fromFile(path, "UTF-8")))(s => ZIO.succeed(s.close())) { src =>
      ZIO.attempt(src.mkString)
    }

  /** Чистое преобразование распарсенного JSON в наш Report */
  private def jsonToReport(j: SimpleJson.Value): Report =
    val obj = j.asObject

    val srcObj = obj("source").asObject
    val source = SourceInfo(
      path            = srcObj("path").asString,
      sampleRate      = srcObj("sampleRate").asInt,
      channels        = srcObj("channels").asInt,
      bitsPerSample   = srcObj("bitsPerSample").asInt,
      durationSeconds = srcObj("durationSeconds").asDouble,
      numSamples      = srcObj("numSamples").asLong
    )

    val anObj = obj("analysis").asObject
    val analysis = AnalysisInfo(
      fftSize          = anObj("fftSize").asInt,
      framesCount      = anObj("framesCount").asInt,
      frameDurationSec = anObj("frameDurationSec").asDouble,
      bpm              = anObj("bpm").asDouble,
      beatsCount       = anObj("beatsCount").asInt
    )

    val beats = obj("beats").asArray.map { v =>
      val o = v.asObject
      Beat(o("frameIndex").asInt, o("timeSec").asDouble, o("energy").asDouble)
    }

    val bands = obj("bands").asArray.map { v =>
      val o = v.asObject
      BandSnapshot(
        timeSec       = o("timeSec").asDouble,
        bass          = o("bass").asDouble,
        mid           = o("mid").asDouble,
        high          = o("high").asDouble,
        dominantFreq  = o("dominantFreq").asDouble
      )
    }

    Report(source, analysis, beats, bands)
