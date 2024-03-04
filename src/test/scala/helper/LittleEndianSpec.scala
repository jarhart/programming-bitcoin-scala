package helper

import helper.*
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LittleEndianSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("toInt"):
    it("converts little-endian bytes to a BigInt"):
      val cases = Table(
        ("hex", "expected"),
        "99c3980000000000" -> BigInt(10011545),
        "a135ef0100000000" -> BigInt(32454049)
      )

      forAll(cases): (hex, expected) =>
        assert(LittleEndian.toInt(parseHex(hex)) == expected)

    it("is the inverse of fromInt"):
      forAll(Gen.posNum[Long]): n =>
        val i = BigInt(n)
        assert(LittleEndian.toInt(LittleEndian.fromInt(i, 8)) == i)

  describe("fromInt converts a BigInt to little-endian bytes"):
    it("fromInt converts a BigInt to little-endian bytes"):
      val cases = Table(
        ("n", "length", "expected"),
        (BigInt(1), 4, "01000000"),
        (BigInt(10011545), 8, "99c3980000000000")
      )

      forAll(cases): (n, length, expected) =>
        assert(formatHex(LittleEndian.fromInt(n, length)) == expected)
