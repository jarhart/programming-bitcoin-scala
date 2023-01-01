package ecc

import math.floorMod

final case class FieldElement[P <: Int](val num: Int)(using ev: ValueOf[P]):
  assert(num >= 0 && num < prime, s"Num ${num} not in field range 0 to ${prime - 1}")

  def prime: Int = valueOf[P]

  def +(that: FieldElement[P]) = mod_p(this.num + that.num)

  def -(that: FieldElement[P]) = mod_p(this.num - that.num)

  def *(that: FieldElement[P]) = mod_p(this.num * that.num)

  def /(that: FieldElement[P]) = this * that.pow(prime - 2)

  def pow(exp: Int) = FieldElement[P](modPow(num, exp, prime))

  private def mod_p(i: Int) = FieldElement[P](floorMod(i, prime))
