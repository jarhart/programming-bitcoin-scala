package helper

object LittleEndian:
  def toInt(bytes: Iterable[Byte]) = unsignedFromBytes(bytes.toArray.reverse)
  def fromInt(i: BigInt, length: Int) = toBytes(i, length).reverse
