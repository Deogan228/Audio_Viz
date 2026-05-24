package monads

// Паттерн «читатель»: вычисления зависят от окружения, которое незримо передаётся
final case class Reader[Env, A](run: Env => A):
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

object Reader:
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)
  def ask[Env]: Reader[Env, Env] = Reader(identity)
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)

  given readerMonad[Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A): Reader[Env, A] = Reader.pure(a)
    def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
      ma.flatMap(f)
