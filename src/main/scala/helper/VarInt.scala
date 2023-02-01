package helper

import cats.syntax.flatMap._

object VarInt:
  private val min2bytes = BigInt(0xfd)
  private val min4bytes = BigInt(2).pow(16)
  private val min8bytes = BigInt(2).pow(32)
  private val minTooBig = BigInt(2).pow(64)

  val decode: Decoder[BigInt] = for {
    b <- Decoder.head map unsigned
    i <- b match
      case 0xff => LittleEndian.decode(8)
      case 0xfe => LittleEndian.decode(4)
      case 0xfd => LittleEndian.decode(2)
      case _ => Decoder.pure(BigInt(b))
  } yield i

  def encode(i: BigInt): Encoder[Unit] =
    require(i < minTooBig, s"integer too large: ${i}")

    if i < min2bytes then
      Encoder.tell(i.toByte)
    else if i < min4bytes then
      Encoder.tell(0xfd.toByte) >> LittleEndian.encode(i, 2)
    else if i < min8bytes then
      Encoder.tell(0xfe.toByte) >> LittleEndian.encode(i, 4)
    else
      Encoder.tell(0xff.toByte) >> LittleEndian.encode(i, 8)

  def read(bytes: LazyList[Byte]): (BigInt, LazyList[Byte]) =
    decode.run(bytes).value.swap

  val parse = Decoder.run(decode)

  def serialize(i: BigInt) = encode(i).written.toArray
