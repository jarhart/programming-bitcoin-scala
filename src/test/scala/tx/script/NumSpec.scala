package tx.script

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import helper.*

class NumSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("Num.encode"):
    it("encodes 0 as no bytes"):
      assert(Num.encode(0).isEmpty)

    it("encodes positive numbers"):
      assert(formatHex(Num.encode(234)) == "ea00")
      assert(formatHex(Num.encode(34567)) == "078700")

    it("encodes negative numbers"):
      assert(formatHex(Num.encode(-234)) == "ea80")
      assert(formatHex(Num.encode(-34567)) == "078780")

  describe("Num.decode"):
    it("Num.decode is the inverse of Num.encode"):
      forAll: (i: Int) =>
        val num = BigInt(i)
        assert(Num.decode(Num.encode(num)) == num)
