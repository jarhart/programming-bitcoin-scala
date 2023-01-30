package helper

import cats.data.State
import cats.syntax.traverse._

type Decoder[A] = State[LazyList[Byte], A]

object Decoder:

  def run[A](da: Decoder[A])(input: IterableOnce[Byte]): A =
    da.runA(LazyList from input).value

  def apply[A](f: LazyList[Byte] => (LazyList[Byte], A)): Decoder[A] = State(f)

  def pure[A](x: A): Decoder[A] = State.pure(x)

  val get: Decoder[LazyList[Byte]] = State.get

  val head: Decoder[Byte] = Decoder(bytes => (bytes.tail, bytes.head))

  def take(n: Int): Decoder[LazyList[Byte]] = Decoder(_.splitAt(n).swap)

  def times[A](n: Int)(da: Decoder[A]): Decoder[LazyList[A]] =
    LazyList.fill(n)(da).sequence

  def many[A](length: Int)(da: Decoder[A]): Decoder[LazyList[A]] =
    take(length) map (LazyList.unfold(_)(bytes =>
      Option.when(bytes.nonEmpty)(da.run(bytes).value.swap)))
