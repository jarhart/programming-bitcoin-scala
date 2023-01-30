package ecc

import helper._
import java.util.HexFormat

object Base58:

  val Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  def encode(bytes: Array[Byte]): String =
    ("1" * bytes.takeWhile(_ == 0).length) ++
    LazyList.unfold(unsignedFromBytes(bytes))(n =>
      Option.when(n > 0)(n /% 58 match
        case (num, mod) => (Alphabet(mod.toInt), num))
    ).foldRight("")((c, result) => result :+ c)

  def decode(s: String): Array[Byte] =
    val num = s.foldLeft(BigInt(0))((num, c) => num * 58 + Alphabet.indexWhere(_ == c))
    num.toByteArray


object Base58check:

  val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  def encode(bytes: Array[Byte]): String =
    Base58.encode(bytes ++ hash256(bytes).take(4))

  def decode(s: String): Array[Byte] =
    val (result, checksum) = padLeft(25)(Base58.decode(s)).splitAt(21)
    val h = hash256(result).take(4)
    require(h.toSeq == checksum.toSeq, s"bad address: ${formatHex(checksum)} ${formatHex(h)}")
    result
