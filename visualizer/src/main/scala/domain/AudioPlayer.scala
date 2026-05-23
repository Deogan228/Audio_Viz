package domain

import zio.{ZIO, Task, Scope}

import javax.sound.sampled.{AudioSystem, Clip, AudioInputStream}
import java.io.File

/** Воспроизведение WAV-файла через javax.sound.sampled.
  *
  * Используется для синхронизации визуализации со звуком: у Clip можно
  * запросить текущую позицию воспроизведения в микросекундах.
  *
  * Блок 4 (IO → ZIO): открытие/закрытие Clip — побочные эффекты. Clip
  * предоставляется как ZIO-ресурс: ZIO.acquireRelease гарантирует, что
  * clip.close() вызовется при завершении Scope, даже если визуализация
  * упала с ошибкой. Это надёжнее ручного stop() из исходной версии.
  */
object AudioPlayer:

  /** Хэндл плеера — тонкая обёртка над Clip с чистыми запросами. */
  final case class PlayerHandle(clip: Clip):
    /** Текущая позиция воспроизведения в секундах. */
    def positionSeconds: Task[Double] =
      ZIO.attempt(clip.getMicrosecondPosition / 1_000_000.0)

    /** Идёт ли воспроизведение. */
    def isRunning: Task[Boolean] =
      ZIO.attempt(clip.isRunning)

  /** Открыть WAV и начать воспроизведение. Ресурс: при выходе из Scope
    * клип останавливается и закрывается автоматически.
    */
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
