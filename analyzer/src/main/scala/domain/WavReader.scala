package domain

import zio.{ZIO, Task}

import java.io.FileInputStream
import java.nio.{ByteBuffer, ByteOrder}

object WavReader:

  def read(path: String): Task[(WavData, Vector[String])] =
    for
      bytes  <- readAllBytes(path)
      parsed <- ZIO.attempt(parseWav(bytes))
      (header, samples, extraLog) = parsed
      log = Vector(
        s"Загружен файл: $path",
        s"Sample rate: ${header.sampleRate} Hz",
        s"Каналов: ${header.channels}",
        s"Битность: ${header.bitsPerSample} бит",
        s"Длительность: ${"%.2f".format(header.durationSeconds)} сек",
        s"Всего сэмплов: ${header.numSamples}"
      ) ++ extraLog
    yield (WavData(header, samples), log)

  private def readAllBytes(path: String): Task[Array[Byte]] =
    ZIO.acquireReleaseWith(ZIO.attempt(new FileInputStream(path)))(is => ZIO.succeed(is.close())) { is =>
      ZIO.attempt {
        val available = is.available()
        val bytes = new Array[Byte](available)
        var off = 0
        while off < available do
          val r = is.read(bytes, off, available - off)
          if r < 0 then off = available else off += r
        bytes
      }
    }

  private def parseWav(bytes: Array[Byte]): (WavHeader, Vector[Double], Vector[String]) =
    val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

    val riff = readString(buf, 4)
    require(riff == "RIFF", s"Не WAV-файл (ожидалось RIFF, получено $riff)")
    buf.getInt()
    val wave = readString(buf, 4)
    require(wave == "WAVE", s"Не WAV-файл (ожидалось WAVE, получено $wave)")

    skipUntilChunk(buf, "fmt ")
    val fmtSize = buf.getInt()
    val fmtStart = buf.position()

    val audioFormat = buf.getShort()
    val channels = buf.getShort().toInt
    val sampleRate = buf.getInt()
    buf.getInt()      // byte rate
    buf.getShort()    // block align
    val bitsPerSample = buf.getShort().toInt

    val formatInfo = audioFormat match
      case 1  => "PCM"
      case -2 => "Extensible (WAVE_FORMAT_EXTENSIBLE)"
      case 3  => "IEEE float"
      case _  => s"unknown ($audioFormat)"

    val isExtensible = audioFormat == -2
    val effectiveFormat =
      if isExtensible && fmtSize >= 40 then
        buf.getShort()           // cbSize
        buf.getShort()           // valid bits per sample
        buf.getInt()             // channel mask
        val subFormatCode = buf.getShort()  // первые 2 байта GUID
        buf.position(buf.position() + 14)
        subFormatCode
      else audioFormat

    val readSoFar = buf.position() - fmtStart
    if fmtSize > readSoFar then
      buf.position(fmtStart + fmtSize)

    require(
      effectiveFormat == 1,
      s"Поддерживается только PCM. Формат заголовка: $formatInfo, эффективный код: $effectiveFormat"
    )
    require(bitsPerSample == 16, s"Поддерживается только 16 бит, получено $bitsPerSample")

    skipUntilChunk(buf, "data")
    val dataSize = buf.getInt()

    val bytesPerSample = bitsPerSample / 8
    val totalSamples = dataSize / bytesPerSample
    val samplesPerChannel = totalSamples / channels

    val samples = Vector.tabulate(samplesPerChannel) { _ =>
      val sumOverChannels = (0 until channels).map(_ => buf.getShort().toDouble).sum
      (sumOverChannels / channels) / 32768.0
    }

    val header = WavHeader(sampleRate, channels, bitsPerSample, samplesPerChannel)
    val extraLog = Vector(s"Формат: $formatInfo")
    (header, samples, extraLog)

  private def readString(buf: ByteBuffer, n: Int): String =
    val arr = new Array[Byte](n)
    buf.get(arr)
    new String(arr, "ASCII")

  private def skipUntilChunk(buf: ByteBuffer, target: String): Unit =
    while buf.remaining() >= 8 do
      val id = readString(buf, 4)
      if id == target then return
      else
        val size = buf.getInt()
        buf.position(buf.position() + size)
    throw new RuntimeException(s"Чанк '$target' не найден в файле")