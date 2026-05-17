package domain

import monads.IO

import javax.sound.sampled.{AudioSystem, Clip, AudioInputStream}
import java.io.File

/** Воспроизведение WAV-файла через javax.sound.sampled.
  *
  * Используется для синхронизации визуализации со звуком:
  * можно узнать текущую позицию воспроизведения в микросекундах.
  */
object AudioPlayer:

  case class PlayerHandle(clip: Clip):
    /** Текущая позиция воспроизведения в секундах */
    def positionSeconds: Double = clip.getMicrosecondPosition / 1_000_000.0
    def isRunning: Boolean = clip.isRunning
    def stop(): Unit =
      if clip.isRunning then clip.stop()
      clip.close()

  def play(path: String): IO[PlayerHandle] = IO.delay {
    val file = new File(path)
    val audioStream: AudioInputStream = AudioSystem.getAudioInputStream(file)
    val clip = AudioSystem.getClip()
    clip.open(audioStream)
    clip.start()
    PlayerHandle(clip)
  }

  def stop(handle: PlayerHandle): IO[Unit] = IO.delay { handle.stop() }
