package helper

object Bytes:

  def take(n: Int): Decoder[Array[Byte]] = Decoder.take(n) map (_.toArray)

  def takeReverse(n: Int): Decoder[Array[Byte]] = take(n) map (_.reverse)

  def tellReverse(bytes: Array[Byte]) = Encoder tell bytes.reverse
