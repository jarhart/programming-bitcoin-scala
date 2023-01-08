package ecc

object LittleEndian:
  def toInt(bytes: Array[Byte]) = BigInt(bytes.reverse)
  def fromInt(i: BigInt, length: Int) = toBytes(i, length).reverse
