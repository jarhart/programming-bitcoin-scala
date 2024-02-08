package ecc

import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Base58Spec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("Base58"):

    describe("encode"):
      it("encodes in base58 format"):
        val bytes = BigInt("987654321987654321").toByteArray
        assert(Base58.encode(bytes) == "3HyFRGmiNAk")

    describe("decode"):
      it("decodes base58 format"):
        val i = BigInt("987654321987654321")
        assert(BigInt(Base58.decode("3HyFRGmiNAk")) == i)

      it("is the inverse of encode"):
        forAll(Gen.posNum[Long]): n =>
          val i = BigInt(n)
          assert(BigInt(Base58.decode(Base58.encode(i.toByteArray))) == i)
