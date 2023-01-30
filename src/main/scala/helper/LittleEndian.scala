package helper

object LittleEndian:

  def decode(length: Int): Decoder[BigInt] =
    Decoder take length map toInt

  def encode(i: BigInt, length: Int) =
    Encoder tell fromInt(i, length)

  def parse(length: Int) = Decoder.run(decode(length))

  def serialize(i: BigInt, length: Int) = encode(i, length).written.toArray

  def toInt(bytes: Iterable[Byte]) = unsignedFromBytes(bytes.toArray.reverse)

  def fromInt(i: BigInt, length: Int) = toBytes(i, length).reverse
