package helper

import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import helper.*

class LittleEndianSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("toIn"):
    it("converts little-endian bytes to a BigInt"):
      val cases = Seq(
        "99c3980000000000" -> BigInt(10011545),
        "a135ef0100000000" -> BigInt(32454049)
      )

      for ((h, expected) <- cases)
        assert(LittleEndian.toInt(parseHex(h)) == expected)

    it("is the inverse of fromInt"):
      forAll(Gen.posNum[Long]): n =>
        val i = BigInt(n)
        assert(LittleEndian.toInt(LittleEndian.fromInt(i, 8)) == i)

  describe("fromInt converts a BigInt to little-endian bytes"):
    it("fromInt converts a BigInt to little-endian bytes"):
      val cases = Seq(
        (BigInt(1), 4, "01000000"),
        (BigInt(10011545), 8, "99c3980000000000")
      )

      for ((n, length, expected) <- cases)
        assert(formatHex(LittleEndian.fromInt(n, length)) == expected)
