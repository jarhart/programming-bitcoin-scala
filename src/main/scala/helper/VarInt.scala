package helper

object VarInt:
  import helper.{Parser => P, Serializer => S}

  private val min2bytes = BigInt(0xfd)
  private val min4bytes = BigInt(2).pow(16)
  private val min8bytes = BigInt(2).pow(32)
  private val minTooBig = BigInt(2).pow(64)

  val parse = for {
    b <- P.head
    i <- unsigned(b) match
      case 0xff => LittleEndian.parse(8)
      case 0xfe => LittleEndian.parse(4)
      case 0xfd => LittleEndian.parse(2)
      case _ => P.pure(BigInt(unsigned(b)))
  } yield i

  def serialize(i: BigInt) =
    require(i < minTooBig, s"integer too large: ${i}")

    if i < min2bytes then
      S.tell(i.toByte)
    else if i < min4bytes then
      S.tell(0xfd.toByte) >> LittleEndian.serialize(i, 2)
    else if i < min8bytes then
      S.tell(0xfe.toByte) >> LittleEndian.serialize(i, 4)
    else
      S.tell(0xff.toByte) >> LittleEndian.serialize(i, 8)

  def read(bytes: LazyList[Byte]): (BigInt, LazyList[Byte]) = parse.run(bytes)

  def encode(i: BigInt): Array[Byte] = serialize(i).toArray
