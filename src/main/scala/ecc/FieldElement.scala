package ecc

final case class FieldElement[P <: BigInt: ValueOf](val num: BigInt):
  require(
    num >= 0 && num < prime,
    s"Num ${num} not in field range 0 to ${prime - 1}"
  )

  def prime: BigInt = valueOf[P]

  def +(that: FieldElement[P]) = mod_p(this.num + that.num)

  def -(that: FieldElement[P]) = mod_p(this.num - that.num)

  def *(that: FieldElement[P]) = mod_p(this.num * that.num)

  def *(x: BigInt) = mod_p(this.num * x)

  def /(that: FieldElement[P]) = this * that.pow(prime - 2)

  infix def pow(exp: BigInt) = FieldElement[P](num.modPow(exp, prime))

  infix def rmul(coeff: BigInt) = mod_p(this.num * coeff)

  private def mod_p(i: BigInt) = FieldElement[P](i mod prime)

given [P <: BigInt: ValueOf]: Coordinate[FieldElement[P]] with
  def add(a: FieldElement[P], b: FieldElement[P]) = a + b
  def sub(a: FieldElement[P], b: FieldElement[P]) = a - b
  def mul(a: FieldElement[P], b: FieldElement[P]) = a * b
  def div(a: FieldElement[P], b: FieldElement[P]) = a / b
  def ipow(a: FieldElement[P], e: Int) = a pow e
  def rmul(coeff: BigInt, e: FieldElement[P]) = e rmul coeff
