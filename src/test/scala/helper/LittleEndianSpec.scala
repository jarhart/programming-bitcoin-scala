package helper

import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.HexFormat

class LittleEndianSpec extends AnyFreeSpec with ScalaCheckPropertyChecks:

  val hexFormat = HexFormat.of()
  import hexFormat.{formatHex, parseHex}

  "toInt converts little-endian bytes to a BigInt" in {
    val cases = Seq(
      "99c3980000000000" -> BigInt(10011545),
      "a135ef0100000000" -> BigInt(32454049),
    )

    for ((h, expected) <- cases)
      assert(LittleEndian.toInt(parseHex(h)) == expected)
  }

  "fromInt converts a BigInt to little-endian bytes" in {
    val cases = Seq(
      (BigInt(1), 4, "01000000"),
      (BigInt(10011545), 8, "99c3980000000000"),
    )

    for ((n, length, expected) <- cases)
      assert(formatHex(LittleEndian.fromInt(n, length)) == expected)
  }

  "toInt is the inverse of fromInt" in {
    forAll(Gen.posNum[Long]) { n =>
      val i = BigInt(n)
      assert(LittleEndian.toInt(LittleEndian.fromInt(i, 8)) == i)
    }
  }
