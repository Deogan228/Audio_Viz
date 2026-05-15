package monads

/** Reader[Env, A] = функция Env => A, обёрнутая в монаду.
  * Используется для проброса конфигурации без явной передачи параметров.
  */
final case class Reader[Env, A](run: Env => A):
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

object Reader:
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)

  /** Получить весь Env целиком */
  def ask[Env]: Reader[Env, Env] = Reader(identity)

  /** Получить часть Env через геттер */
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)