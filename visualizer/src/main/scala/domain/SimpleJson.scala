package domain

// Свой маленький парсер JSON — без зависимостей, умеет всё, что нужно для отчёта
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

  /** Рекурсивный парсер */
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
