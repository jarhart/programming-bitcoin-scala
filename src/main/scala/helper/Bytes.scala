package helper

object Bytes:

  infix def take(n: Int): Decoder[Array[Byte]] = Decoder.take(n) map (_.toArray)

  infix def takeReverse(n: Int): Decoder[Array[Byte]] = take(n) map (_.reverse)

  def tellReverse(bytes: Array[Byte]) = Encoder.tell(bytes.reverse)
