package ecc

import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.HexFormat

import helper.toBytes
class Base58CheckSpec extends AnyFreeSpec with ScalaCheckPropertyChecks:

  val hexFormat = HexFormat.of()
  import hexFormat.{formatHex, parseHex}

  "Base58check" - {
    "encode encodes in base58check format" in {
      val examples = Seq(
        "008abeb2568398324ca2b898bdf8ea5f0a5a7a78a1" -> "1DechJoU257M5xB5wBVHRLQsZTjmYn1s9D",
        "00aaef4ba40dc7064361e977e4e1b3462b2da74c36" -> "1GapY9pVhsE5mfdAk7agW9KD7ReD63RN9A",
        "002669e231e7f60d50e7e5db605b73399f343acd9d" -> "14W7XD8vrBSUF59cJGMKAjqVM1ZQaKgLGU",
        "0061ae62a9b30603497ba4f1596b94322cb83670c2" -> "19uVT7pv64D3VAKBdDYbgaQBap252ojZPr",
        "00f595534ddde89f4b6e2b187126a53ebce5244a33" -> "1PPXSJxYn1JCfAQkQhDw3YHe2BYr3pjpvz",
      )

      for ((hex, base58check) <- examples)
        assert(Base58check.encode(parseHex(hex)) == base58check)
    }

    "decode decodes base58 format" in {
      val examples = Seq(
        "13WbsoHJNftbfcqrt5crnAYEJvHyQ5vZHP" -> "001b896d65b9c7ed4365c4f5ba2392c4a62c04a7ab",
        "16M3X4uLXvFf3zFXzKoZRfWmPY8TEfLJXP" -> "003aa30ce7b095251f377e73e1dff993bd85e545a2",
        "143PV2hGMugq7zdrJDrUGu4XCaJC6Wvp63" -> "00215bfa3412f428d80e9e4b8e41b54e5a01697ca0",
        "1663uoQZfLKvMkN1Q6AL4We552DY18Loft" -> "0037cd235ea4e4fcb1a5cc288af1674a1038164761",
        "16TCJznrf6nSJhYtyL8jfQyHoxE787SqeU" -> "003bcce2a14e9965b4315a36f5b06460fd062f60f7",
      )

      for ((base58check, hex) <- examples)
        assert(formatHex(Base58check.decode(base58check)) == hex)
    }

    "decode is the inverse of encode" in {
      forAll(Gen.posNum[Long]) { n =>
        val i = BigInt(n)
        assert(BigInt(Base58check.decode(Base58check.encode(toBytes(i, 21)))) == i)
      }
    }
  }
