package monads

/** Writer[Log, A] = пара (лог, значение).
  * Лог накапливается при последовательных flatMap.
  * Log должен иметь моноидную структуру (для нас — Vector[String]).
  */
final case class Writer[Log, A](log: Log, value: A)(using ml: Monoid[Log]):
  def flatMap[B](f: A => Writer[Log, B]): Writer[Log, B] =
    val next = f(value)
    Writer(ml.combine(log, next.log), next.value)

  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

object Writer:
  def pure[Log: Monoid, A](a: A): Writer[Log, A] =
    Writer(summon[Monoid[Log]].empty, a)

  /** Записать сообщение в лог, не меняя значение */
  def tell[Log: Monoid](entry: Log): Writer[Log, Unit] =
    Writer(entry, ())

/** Минимальный моноид — для абстракции над "склейкой" лога */
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object Monoid:
  given vectorStringMonoid: Monoid[Vector[String]] with
    def empty: Vector[String] = Vector.empty
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y