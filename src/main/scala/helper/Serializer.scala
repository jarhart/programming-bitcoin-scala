package helper

object Serializer:

  def pure[A](a: A) = Serializer[A]((a, _))

  def tell(byte: Byte) = Serializer(out => (byte, out :+ byte))

  def tell(bytes: Array[Byte]) = Serializer(out => (bytes, out concat bytes))
  
  def tell[B <: IterableOnce[Byte]](bytes: B) = Serializer(out => (bytes, out concat bytes))

  def traverse[A, B](xs: Seq[A])(f: A => Serializer[B]): Serializer[Seq[B]] =
    (xs foldLeft pure(Seq()))((sAcc, x) =>
      for (acc <- sAcc; a <- f(x)) yield acc :+ a)

class Serializer[A](val run: LazyList[Byte] => (A, LazyList[Byte])) extends (() => LazyList[Byte]):

  override def apply(): LazyList[Byte] =
    run(LazyList.empty[Byte]) match
      case (_, out) => out

  def toArray: Array[Byte] = apply().toArray

  def map[B](f: A => B) = Serializer[B](
    run(_) match
      case (a, out) => (f(a), out))

  def flatMap[B](f: A => Serializer[B]) = Serializer[B](
    run(_) match
      case (a, out) => f(a).run(out)
  )

  def >>[B](next: => Serializer[B]) = flatMap(_ => next)
