package helper

object Base58:

  val Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  def encode(bytes: Array[Byte]): String =
    ("1" * bytes.takeWhile(_ == 0).length) ++
      LazyList
        .unfold(unsignedFromBytes(bytes)): n =>
          Option.when(n > 0):
            n /% 58 match
              case (num, mod) => (Alphabet(mod.toInt), num)
        .foldRight(""): (c, result) =>
          result :+ c

  def decode(s: String): Array[Byte] =
    val bytes =
      s
        .foldLeft(BigInt(0)): (num, c) =>
          num * 58 + Alphabet.indexWhere(_ == c)
        .toByteArray

    bytes.slice(1, bytes.length - 4)

object Base58check:

  def encode(bytes: Array[Byte]): String =
    Base58.encode(bytes ++ hash256(bytes).take(4))
