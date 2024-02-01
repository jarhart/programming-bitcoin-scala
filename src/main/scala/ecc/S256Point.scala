package ecc

import helper.*

type S256Point = Point[S256Field, S256Point.A, S256Point.B]

object S256Point:

  val a = S256Field(0)
  val b = S256Field(7)

  type A = a.type
  type B = b.type

  val n = BigInt(
    "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141",
    16
  )

  val g =
    S256Point(
      x = BigInt(
        "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
        16
      ),
      y = BigInt(
        "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",
        16
      )
    )

  def atInfinity: S256Point = Point.atInfinity

  def apply(x: S256Field, y: S256Field): S256Point = Point(x, y)
  def apply(x: BigInt, y: BigInt): S256Point = Point(S256Field(x), S256Field(y))

  def verify(self: S256Point, z: BigInt, sig: Signature): Boolean =
    val sInv = sig.s.modPow(n - 2, n)
    val u = (z * sInv) mod n
    val v = (sig.r * sInv) mod n
    u * g + v * self match
      case NonZeroPoint(x, _) => x.num == sig.r
      case _                  => false

  def sec(self: S256Point, compressed: Boolean = true): Array[Byte] = self match
    case NonZeroPoint(x, y) =>
      if compressed then
        if y.num.mod(2) == 0 then (2: Byte) +: toBytes(x.num)
        else (3: Byte) +: toBytes(x.num)
      else (4: Byte) +: (toBytes(x.num) ++ toBytes(y.num))
    case _ => ???

  def hash160(self: S256Point, compressed: Boolean = true): Array[Byte] =
    helper.hash160(sec(self, compressed))

  def address(
      self: S256Point,
      compressed: Boolean = true,
      testnet: Boolean = false
  ) =
    val prefix: Byte = if testnet then 0x6f else 0
    Base58check.encode(prefix +: S256Point.hash160(self, compressed))

  def parse(sec: Array[Byte]): Option[S256Point] = sec match

    case Array[Byte](4, rest*) =>
      rest.toArray splitAt 32 match
        case (xBin, yBin) =>
          val x = unsignedFromBytes(xBin)
          val y = unsignedFromBytes(yBin)
          Some(S256Point(x, y))

    case Array[Byte](marker @ (2 | 3), rest*) =>
      val x = S256Field(unsignedFromBytes(rest.toArray))
      val alpha = x.pow(3) + b
      val beta = alpha.sqrt()
      val betaInv = S256Field(S256Field.p - beta.num)
      val (evenBeta, oddBeta) =
        if beta.num.mod(2) == 0 then (beta, betaInv) else (betaInv, beta)
      val y = if marker == 2 then evenBeta else oddBeta
      Some(S256Point(x, y))

    case _ =>
      None

  given RMul[S256Point] with
    def rmul(coeff: BigInt, e: S256Point) = e.rmul(coeff mod n)

extension (self: S256Point)

  def verify(z: BigInt, sig: Signature) = S256Point.verify(self, z, sig)

  def sec(compressed: Boolean = true) = S256Point.sec(self, compressed)

  def address(compressed: Boolean = true, testnet: Boolean = false) =
    S256Point.address(self, compressed = compressed, testnet = testnet)
