package monads

/** Блок 0. Наивный IO[A] — "ленивое" описание побочного эффекта.
  *
  * Эффект не выполняется, пока не вызван unsafeRun. Это разделение
  * описания и исполнения — ключевая идея ФП.
  *
  * ВНИМАНИЕ: реализация намеренно наивная (как просит ТЗ — "наивный IO
  * с маленьким unsafeRun"). Она НЕ имеет trampoline, поэтому длинная
  * цепочка flatMap переполнит стек. Именно поэтому в реальном сценарии
  * анализатора (app/Main.scala) используется ZIO, а этот IO остаётся
  * учебной демонстрацией блока 0.
  */
final class IO[A](val unsafeRun: () => A):
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

object IO:
  def apply[A](thunk: () => A): IO[A] = new IO(thunk)

  /** Поднять чистое значение в IO */
  def pure[A](a: A): IO[A] = IO(() => a)

  /** Захватить произвольный побочный эффект */
  def delay[A](thunk: => A): IO[A] = IO(() => thunk)

  /** Готовые операции с консолью */
  def println(s: String): IO[Unit] = delay(Predef.println(s))
  def readLine: IO[String] = delay(scala.io.StdIn.readLine())

  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A] = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
