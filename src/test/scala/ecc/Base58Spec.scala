package ecc

import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Base58Spec extends AnyFreeSpec with ScalaCheckPropertyChecks:

  "Base58" - {
    "encode encodes in base58 format" in:
      val bytes = BigInt("987654321987654321").toByteArray
      assert(Base58.encode(bytes) == "3HyFRGmiNAk")

    "decode decodes base58 format" in:
      val i = BigInt("987654321987654321")
      assert(BigInt(Base58.decode("3HyFRGmiNAk")) == i)

    "decode is the inverse of encode" in:
      forAll(Gen.posNum[Long]): n =>
        val i = BigInt(n)
        assert(BigInt(Base58.decode(Base58.encode(i.toByteArray))) == i)
  }
