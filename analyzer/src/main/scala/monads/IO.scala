package monads

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

  given ioMonad: Monad[IO] with
    def pure[A](a: A): IO[A] = IO.pure(a)
    def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
