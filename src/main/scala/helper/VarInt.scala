package helper

object VarInt:

  private val min2bytes = BigInt(0xfd)
  private val min4bytes = BigInt(2).pow(16)
  private val min8bytes = BigInt(2).pow(32)
  private val minTooBig = BigInt(2).pow(64)

  def read(bytes: LazyList[Byte]): (BigInt, LazyList[Byte]) =
    unsigned(bytes.head) match
      case 0xff => bytes.tail.splitAt(8)
      case 0xfe => bytes.tail.splitAt(4)
      case 0xfd => bytes.tail.splitAt(2)
      case _ => bytes.splitAt(1)
    match
      case (varIntBytes, rest) => (LittleEndian.toInt(varIntBytes), rest)

  def encode(i: BigInt): Array[Byte] =
    require(i < minTooBig, s"integer too large: ${i}")

    if i < min2bytes then
      Array(i.toByte)
    else if i < min4bytes then
      0xfd.toByte +: LittleEndian.fromInt(i, 2)
    else if i < min8bytes then
      0xfe.toByte +: LittleEndian.fromInt(i, 4)
    else
      0xff.toByte +: LittleEndian.fromInt(i, 8)
