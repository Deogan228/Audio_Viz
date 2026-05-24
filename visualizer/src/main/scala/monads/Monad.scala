package monads

// Простейшая абстракция монады: умеет pure и flatMap.
trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

object Monad:
  def apply[M[_]](using m: Monad[M]): Monad[M] = m

  extension [M[_], A](ma: M[A])(using m: Monad[M])
    def flatMap[B](f: A => M[B]): M[B] = m.flatMap(ma)(f)
    def map[B](f: A => B): M[B] = m.map(ma)(f)
