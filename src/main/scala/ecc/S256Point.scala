package ecc

type S256Point = Point[S256Field, S256Point.A, S256Point.B]

object S256Point:

  val a = S256Field(0)
  val b = S256Field(7)

  val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  type A = a.type
  type B = b.type

  val g =
    S256Point(
      x = BigInt("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798", 16),
      y = BigInt("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16))

  def atInfinity: S256Point = Point.atInfinity

  def apply(x: S256Field, y: S256Field): S256Point = Point(x, y)
  def apply(x: BigInt, y: BigInt): S256Point = Point(S256Field(x), S256Field(y))

  given RMul[S256Point] with
    def rmul(coeff: BigInt, e: S256Point) = e.rmul(coeff mod n)

extension (self: S256Point)
  def verify(z: BigInt, sig: Signature) =
    val sInv = sig.s.modPow(S256Point.n - 2, S256Point.n)
    val u = (z * sInv) mod S256Point.n
    val v = (sig.r * sInv) mod S256Point.n
    u * S256Point.g + v * self match {
      case NonZeroPoint(x, _) => x.num == sig.r
      case _ => false
    }
