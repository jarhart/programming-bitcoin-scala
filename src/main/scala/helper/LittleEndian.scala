package helper

object LittleEndian:

  def parse(n: Int) = Parser take n map toInt

  def serialize(i: BigInt, length: Int) = Serializer tell fromInt(i, length)

  def toInt(bytes: Iterable[Byte]) = unsignedFromBytes(bytes.toArray.reverse)

  def fromInt(i: BigInt, length: Int) = toBytes(i, length).reverse
