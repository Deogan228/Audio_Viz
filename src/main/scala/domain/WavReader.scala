package domain

import monads.{IO, Writer, Monoid}
import monads.Monoid.given

import java.io.{FileInputStream, DataInputStream}
import java.nio.{ByteBuffer, ByteOrder}

/** Парсер WAV-файлов (PCM, 16 бит).
  *
  * WAV это RIFF-контейнер:
  *   "RIFF" + size + "WAVE"
  *   "fmt " + chunk_size + параметры формата
  *   "data" + chunk_size + сэмплы
  */
object WavReader:

  /** Читает WAV-файл и возвращает данные + лог операций.
    * IO оборачивает побочный эффект (чтение с диска),
    * Writer накапливает диагностические сообщения.
    */
  def read(path: String): IO[Writer[Vector[String], WavData]] = IO.delay {
    val bytes = readAllBytes(path)
    val (header, samples) = parseWav(bytes)

    val log = Vector(
      s"Загружен файл: $path",
      s"Sample rate: ${header.sampleRate} Hz",
      s"Каналов: ${header.channels}",
      s"Битность: ${header.bitsPerSample} бит",
      s"Длительность: ${"%.2f".format(header.durationSeconds)} сек",
      s"Всего сэмплов: ${header.numSamples}"
    )

    Writer(log, WavData(header, samples))
  }

  private def readAllBytes(path: String): Array[Byte] =
    val is = new FileInputStream(path)
    try
      val dis = new DataInputStream(is)
      val bytes = new Array[Byte](is.available())
      dis.readFully(bytes)
      bytes
    finally is.close()

  private def parseWav(bytes: Array[Byte]): (WavHeader, Vector[Double]) =
    val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

    // "RIFF"
    val riff = readString(buf, 4)
    require(riff == "RIFF", s"Не WAV-файл (ожидалось RIFF, получено $riff)")
    buf.getInt()  // размер файла - 8, нам не нужен
    val wave = readString(buf, 4)
    require(wave == "WAVE", s"Не WAV-файл (ожидалось WAVE, получено $wave)")

    // Ищем чанк "fmt "
    skipUntilChunk(buf, "fmt ")
    val fmtSize = buf.getInt()
    val audioFormat = buf.getShort()       // 1 = PCM
    require(audioFormat == 1, s"Поддерживается только PCM, получен формат $audioFormat")
    val channels = buf.getShort().toInt
    val sampleRate = buf.getInt()
    buf.getInt()                            // byte rate, пропускаем
    buf.getShort()                          // block align, пропускаем
    val bitsPerSample = buf.getShort().toInt
    // Пропускаем хвост fmt-чанка если он длиннее 16 байт
    if fmtSize > 16 then buf.position(buf.position() + (fmtSize - 16))

    // Ищем чанк "data"
    skipUntilChunk(buf, "data")
    val dataSize = buf.getInt()

    require(bitsPerSample == 16, s"Поддерживается только 16 бит, получено $bitsPerSample")

    val bytesPerSample = bitsPerSample / 8
    val totalSamples = dataSize / bytesPerSample
    val samplesPerChannel = totalSamples / channels

    // Читаем сэмплы. Если стерео — усредняем каналы в моно
    val samples = Vector.tabulate(samplesPerChannel) { _ =>
      val sumOverChannels = (0 until channels).map(_ => buf.getShort().toDouble).sum
      val avg = sumOverChannels / channels
      avg / 32768.0  // нормализация в [-1.0, 1.0]
    }

    val header = WavHeader(sampleRate, channels, bitsPerSample, samplesPerChannel)
    (header, samples)

  private def readString(buf: ByteBuffer, n: Int): String =
    val arr = new Array[Byte](n)
    buf.get(arr)
    new String(arr, "ASCII")

  /** Пропускает чанки до тех пор, пока не найдёт нужный */
  private def skipUntilChunk(buf: ByteBuffer, target: String): Unit =
    while buf.remaining() >= 8 do
      val id = readString(buf, 4)
      if id == target then
        // Откатываемся на 4 байта назад чтобы дальше прочитать size
        buf.position(buf.position() - 4)
        return
      else
        val size = buf.getInt()
        buf.position(buf.position() + size)
    throw new RuntimeException(s"Чанк '$target' не найден в файле")