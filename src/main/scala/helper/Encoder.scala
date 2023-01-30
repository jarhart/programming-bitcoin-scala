package helper

import cats.data.Writer

type Encoder[A] = Writer[LazyList[Byte], A]

object Encoder:

  def tell(bytes: IterableOnce[Byte]): Encoder[Unit] = Writer.tell(LazyList from bytes)

  def tell(bytes: Byte*): Encoder[Unit] = tell(bytes)
