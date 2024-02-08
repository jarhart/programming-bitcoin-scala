package ecc

import org.scalatest.funspec.AnyFunSpec

class FieldElementSpec extends AnyFunSpec:

  val p = BigInt(31)
  type P = p.type

  describe("FieldElement"):

    describe("operator +"):

      it("adds the numbers"):
        val a = FieldElement[P](2)
        val b = FieldElement[P](15)
        assert(a + b == FieldElement[P](17))

      it("adds in the field"):
        val a = FieldElement[P](17)
        val b = FieldElement[P](21)
        assert(a + b == FieldElement[P](7))

    describe("operator -"):

      it("subtracts the numbers"):
        val a = FieldElement[P](29)
        val b = FieldElement[P](4)
        assert(a - b == FieldElement[P](25))

      it("subtracts in the field"):
        val a = FieldElement[P](15)
        val b = FieldElement[P](30)
        assert(a - b == FieldElement[P](16))

    describe("operator *"):

      it("multiplies in the field"):
        val a = FieldElement[P](24)
        val b = FieldElement[P](19)
        assert(a * b == FieldElement[P](22))
  
    describe("operator /"):
      it("does field division"):
        val a = FieldElement[P](3)
        val b = FieldElement[P](24)
        assert(a / b == FieldElement[P](4))

    describe("pow"):

      it("does field exponentiation - case 1"):
        val a = FieldElement[P](17)
        assert(a.pow(3) == FieldElement[P](15))

      it("does field exponentiation - case 2"):
        val a = FieldElement[P](5)
        val b = FieldElement[P](18)
        assert(a.pow(5) * b == FieldElement[P](16))

      it("does field exponentiation with negative exponents - case 1"):
        val a = FieldElement[P](17)
        assert(a.pow(-3) == FieldElement[P](29))

      it("does field exponentiation with negative exponents - case 2"):
        val a = FieldElement[P](4)
        val b = FieldElement[P](11)
        assert(a.pow(-4) * b == FieldElement[P](13))
