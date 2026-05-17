package domain

import monads.{IO, Writer, Monoid}
import monads.Monoid.given

import scala.io.Source

/** Парсер JSON-отчёта от анализатора.
  *
  * JSON парсится вручную через простой recursive descent —
  * никаких внешних библиотек. Это нормально, т.к. формат отчёта
  * фиксированный и хорошо известный (см. REPORT_FORMAT.md).
  *
  * IO оборачивает чтение файла с диска,
  * Writer накапливает диагностический лог.
  */
object ReportReader:

  def read(path: String): IO[Writer[Vector[String], Report]] = IO.delay {
    val raw = readFile(path)
    val json = SimpleJson.parse(raw)
    val report = jsonToReport(json)

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
  }

  private def readFile(path: String): String =
    val src = Source.fromFile(path, "UTF-8")
    try src.mkString finally src.close()

  /** Преобразование разобранного JSON в типизированный Report */
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


/** Минимальный JSON-парсер.
  * Поддерживает: объекты, массивы, строки, числа, true/false/null.
  * Достаточно для наших отчётов.
  */
object SimpleJson:

  sealed trait Value:
    def asObject: Map[String, Value] = this match
      case JObject(m) => m
      case other      => throw new RuntimeException(s"Ожидался объект, найдено: $other")
    def asArray: Vector[Value] = this match
      case JArray(a) => a
      case other     => throw new RuntimeException(s"Ожидался массив, найдено: $other")
    def asString: String = this match
      case JString(s) => s
      case other      => throw new RuntimeException(s"Ожидалась строка, найдено: $other")
    def asDouble: Double = this match
      case JNumber(n) => n
      case other      => throw new RuntimeException(s"Ожидалось число, найдено: $other")
    def asInt: Int = asDouble.toInt
    def asLong: Long = asDouble.toLong

  case class JObject(fields: Map[String, Value]) extends Value
  case class JArray(items: Vector[Value])        extends Value
  case class JString(value: String)              extends Value
  case class JNumber(value: Double)              extends Value
  case object JTrue                              extends Value
  case object JFalse                             extends Value
  case object JNull                              extends Value

  def parse(input: String): Value =
    val p = new Parser(input)
    p.skipWs()
    val v = p.parseValue()
    p.skipWs()
    if !p.eof then throw new RuntimeException(s"Ожидался EOF, есть лишний текст в позиции ${p.pos}")
    v

  /** Recursive descent parser */
  private class Parser(val input: String):
    var pos: Int = 0
    def eof: Boolean = pos >= input.length
    def peek: Char = input.charAt(pos)

    def skipWs(): Unit =
      while !eof && peek.isWhitespace do pos += 1

    def expect(c: Char): Unit =
      if eof || peek != c then
        throw new RuntimeException(s"Ожидался '$c' в позиции $pos")
      pos += 1

    def parseValue(): Value =
      skipWs()
      if eof then throw new RuntimeException("Неожиданный EOF")
      peek match
        case '{' => parseObject()
        case '[' => parseArray()
        case '"' => JString(parseString())
        case 't' => parseLiteral("true");  JTrue
        case 'f' => parseLiteral("false"); JFalse
        case 'n' => parseLiteral("null");  JNull
        case c if c == '-' || c.isDigit => JNumber(parseNumber())
        case other => throw new RuntimeException(s"Неожиданный символ '$other' в позиции $pos")

    def parseLiteral(s: String): Unit =
      for c <- s do expect(c)

    def parseObject(): JObject =
      expect('{')
      skipWs()
      val fields = scala.collection.mutable.LinkedHashMap[String, Value]()
      if !eof && peek == '}' then
        pos += 1
        JObject(fields.toMap)
      else
        var continue = true
        while continue do
          skipWs()
          val key = parseString()
          skipWs()
          expect(':')
          val v = parseValue()
          fields(key) = v
          skipWs()
          if !eof && peek == ',' then
            pos += 1
          else
            continue = false
        skipWs()
        expect('}')
        JObject(fields.toMap)

    def parseArray(): JArray =
      expect('[')
      skipWs()
      val items = scala.collection.mutable.ArrayBuffer[Value]()
      if !eof && peek == ']' then
        pos += 1
        JArray(items.toVector)
      else
        var continue = true
        while continue do
          items += parseValue()
          skipWs()
          if !eof && peek == ',' then
            pos += 1
            skipWs()
          else
            continue = false
        skipWs()
        expect(']')
        JArray(items.toVector)

    def parseString(): String =
      expect('"')
      val sb = new StringBuilder
      while !eof && peek != '"' do
        if peek == '\\' then
          pos += 1
          if eof then throw new RuntimeException("Незавершённый escape")
          peek match
            case '"'  => sb.append('"');  pos += 1
            case '\\' => sb.append('\\'); pos += 1
            case '/'  => sb.append('/');  pos += 1
            case 'n'  => sb.append('\n'); pos += 1
            case 't'  => sb.append('\t'); pos += 1
            case 'r'  => sb.append('\r'); pos += 1
            case other => sb.append(other); pos += 1
        else
          sb.append(peek)
          pos += 1
      expect('"')
      sb.toString

    def parseNumber(): Double =
      val start = pos
      if !eof && peek == '-' then pos += 1
      while !eof && peek.isDigit do pos += 1
      if !eof && peek == '.' then
        pos += 1
        while !eof && peek.isDigit do pos += 1
      if !eof && (peek == 'e' || peek == 'E') then
        pos += 1
        if !eof && (peek == '+' || peek == '-') then pos += 1
        while !eof && peek.isDigit do pos += 1
      input.substring(start, pos).toDouble
