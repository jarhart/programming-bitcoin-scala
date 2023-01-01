package ecc

import org.scalatest.freespec.AnyFreeSpec

class FieldElementSpec extends AnyFreeSpec:

  "FieldElement" - {
    "operator +" - {
      "adds the numbers" in {
        val a = FieldElement[31](2)
        val b = FieldElement[31](15)
        assert(a + b == FieldElement[31](17))
      }

      "adds in the field" in {
        val a = FieldElement[31](17)
        val b = FieldElement[31](21)
        assert(a + b == FieldElement[31](7))
      }
    }

    "operator -" - {
      "subtracts the numbers" in {
        val a = FieldElement[31](29)
        val b = FieldElement[31](4)
        assert(a - b == FieldElement[31](25))
      }

      "subtracts in the field" in {
        val a = FieldElement[31](15)
        val b = FieldElement[31](30)
        assert(a - b == FieldElement[31](16))
      }
    }

    "operator *" - {
      "multiplies in the field" in {
        val a = FieldElement[31](24)
        val b = FieldElement[31](19)
        assert(a * b == FieldElement[31](22))
      }
    }
  
    "operator /" - {
      "does field division" in {
        val a = FieldElement[31](3)
        val b = FieldElement[31](24)
        assert(a / b == FieldElement[31](4))
      }
    }

    "pow" - {
      "does field exponentiation case 1" in {
        val a = FieldElement[31](17)
        assert(a.pow(3) == FieldElement[31](15))
      }

      "does field exponentiation case 2" in {
        val a = FieldElement[31](5)
        val b = FieldElement[31](18)
        assert(a.pow(5) * b == FieldElement[31](16))
      }

      "does field exponentiation with negative exponents case 1" in {
        val a = FieldElement[31](17)
        assert(a.pow(-3) == FieldElement[31](29))
      }

      "does field exponentiation with negative exponents case 2" in {
        val a = FieldElement[31](4)
        val b = FieldElement[31](11)
        assert(a.pow(-4) * b == FieldElement[31](13))
      }
    }
  }
