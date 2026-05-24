package monads

final case class State[S, A](run: S => (S, A)):
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      f(a).run(s1)
    }

  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  /** Прочитать текущее состояние */
  def get[S]: State[S, S] = State(s => (s, s))

  /** Полностью перезаписать состояние */
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

  /** Изменить состояние функцией */
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A): State[S, A] = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)
