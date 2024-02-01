package helper

object HexFormat:

  def parseHex(s: String): Array[Byte] =
    s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def formatHex(bytes: Array[Byte]): String =
    bytes.map("%02x".format(_)).mkString
