package ecc

import org.scalatest.freespec.AnyFreeSpec

class FieldElementSpec extends AnyFreeSpec:

  val p = BigInt(31)
  type P = p.type

  "FieldElement" - {
    "operator +" - {
      "adds the numbers" in {
        val a = FieldElement[P](2)
        val b = FieldElement[P](15)
        assert(a + b == FieldElement[P](17))
      }

      "adds in the field" in {
        val a = FieldElement[P](17)
        val b = FieldElement[P](21)
        assert(a + b == FieldElement[P](7))
      }
    }

    "operator -" - {
      "subtracts the numbers" in {
        val a = FieldElement[P](29)
        val b = FieldElement[P](4)
        assert(a - b == FieldElement[P](25))
      }

      "subtracts in the field" in {
        val a = FieldElement[P](15)
        val b = FieldElement[P](30)
        assert(a - b == FieldElement[P](16))
      }
    }

    "operator *" - {
      "multiplies in the field" in {
        val a = FieldElement[P](24)
        val b = FieldElement[P](19)
        assert(a * b == FieldElement[P](22))
      }
    }
  
    "operator /" - {
      "does field division" in {
        val a = FieldElement[P](3)
        val b = FieldElement[P](24)
        assert(a / b == FieldElement[P](4))
      }
    }

    "pow" - {
      "does field exponentiation case 1" in {
        val a = FieldElement[P](17)
        assert(a.pow(3) == FieldElement[P](15))
      }

      "does field exponentiation case 2" in {
        val a = FieldElement[P](5)
        val b = FieldElement[P](18)
        assert(a.pow(5) * b == FieldElement[P](16))
      }

      "does field exponentiation with negative exponents case 1" in {
        val a = FieldElement[P](17)
        assert(a.pow(-3) == FieldElement[P](29))
      }

      "does field exponentiation with negative exponents case 2" in {
        val a = FieldElement[P](4)
        val b = FieldElement[P](11)
        assert(a.pow(-4) * b == FieldElement[P](13))
      }
    }
  }
