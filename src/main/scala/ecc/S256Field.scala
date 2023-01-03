package ecc

type S256Field = FieldElement[S256Field.P]

object S256Field:
  val p = BigInt(2).pow(256) - BigInt(2).pow(32) - 977
  type P = p.type

  def apply(num: BigInt): S256Field = FieldElement(num)
