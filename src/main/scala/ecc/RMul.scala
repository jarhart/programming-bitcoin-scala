package ecc

trait RMul[A]:
  def rmul(coeff: BigInt, x: A): A

extension (coeff: BigInt) def *[A](x: A)(using A: RMul[A]) = A.rmul(coeff, x)
