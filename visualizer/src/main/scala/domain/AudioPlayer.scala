package domain

import zio.{ZIO, Task, Scope}

import javax.sound.sampled.{AudioSystem, Clip, AudioInputStream}
import java.io.File

// Проигрыватель WAV: открываем файл, запускаем, даём текущее время
object AudioPlayer:

  /** Хэндл для общения с плеером */
  final case class PlayerHandle(clip: Clip):
    /** Текущая позиция воспроизведения в секундах */
    def positionSeconds: Task[Double] =
      ZIO.attempt(clip.getMicrosecondPosition / 1_000_000.0)

    /** Играет ли ещё трек. */
    def isRunning: Task[Boolean] =
      ZIO.attempt(clip.isRunning)

  /** Открыть WAV и начать воспроизведение. При завершении автоматически остановим и закроем клип */
  def play(path: String): ZIO[Scope, Throwable, PlayerHandle] =
    ZIO.acquireRelease(start(path)) { handle =>
      ZIO.succeed {
        if handle.clip.isRunning then handle.clip.stop()
        handle.clip.close()
      }
    }

  private def start(path: String): Task[PlayerHandle] =
    ZIO.attempt {
      val file = new File(path)
      val audioStream: AudioInputStream = AudioSystem.getAudioInputStream(file)
      val clip = AudioSystem.getClip()
      clip.open(audioStream)
      clip.start()
      PlayerHandle(clip)
    }
