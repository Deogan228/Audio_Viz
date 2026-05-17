package domain

import monads.IO

import javax.sound.sampled.{AudioSystem, Clip, AudioInputStream}
import java.io.File

/** Воспроизведение WAV-файла через javax.sound.sampled.
  *
  * Используется Clip — он загружает весь файл в память и проигрывает.
  * Подходит для файлов до ~1 минуты, для длинных лучше SourceDataLine,
  * но для курсача Clip вполне достаточно.
  */
object AudioPlayer:

  /** Состояние плеера: запущенный Clip, который можно остановить */
  case class PlayerHandle(clip: Clip):
    def stop(): Unit =
      if clip.isRunning then clip.stop()
      clip.close()

  /** Запускает воспроизведение и возвращает handle для остановки.
    * Воспроизведение асинхронное — IO возвращает управление сразу.
    */
  def play(path: String): IO[PlayerHandle] = IO.delay {
    val file = new File(path)
    val audioStream: AudioInputStream = AudioSystem.getAudioInputStream(file)
    val clip = AudioSystem.getClip()
    clip.open(audioStream)
    clip.start()
    PlayerHandle(clip)
  }

  /** Остановить воспроизведение */
  def stop(handle: PlayerHandle): IO[Unit] = IO.delay {
    handle.stop()
  }