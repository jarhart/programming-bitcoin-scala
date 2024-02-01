package helper

import cats.data.State
import cats.syntax.traverse.*

type Decoder[A] = State[LazyList[Byte], A]

object Decoder:
  def run[A](da: Decoder[A])(input: IterableOnce[Byte]): A =
    da.runA(LazyList from input).value

  def pure[A](x: A): Decoder[A] = State.pure(x)

  def take(n: Int): Decoder[LazyList[Byte]] = State(_.splitAt(n).swap)

  val head: Decoder[Byte] = take(1) map (_.head)

  def times[A](n: Int)(da: Decoder[A]): Decoder[LazyList[A]] =
    LazyList.fill(n)(da).sequence

  def many[A](length: Int)(da: Decoder[A]): Decoder[LazyList[A]] =
    take(length) map:
      LazyList.unfold(_): bytes =>
        Option.when(bytes.nonEmpty)(da.run(bytes).value.swap)
