package monads

/** Наивный IO[A] — "ленивое" описание побочного эффекта.
  * Эффект выполняется только при вызове unsafeRun.
  */
final class IO[A](val unsafeRun: () => A):
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

object IO:
  def apply[A](thunk: () => A): IO[A] = new IO(thunk)
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](thunk: => A): IO[A] = IO(() => thunk)
  def println(s: String): IO[Unit] = delay(Predef.println(s))
  def readLine: IO[String] = delay(scala.io.StdIn.readLine())
