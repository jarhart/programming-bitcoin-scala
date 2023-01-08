package ecc

import org.scalatest.freespec.AnyFreeSpec

class Base58Spec extends AnyFreeSpec:

  "Base58" - {
    "encode encodes in base58 format" in {
      val bytes = BigInt("987654321987654321").toByteArray
      assert(Base58.encode(bytes) == "3HyFRGmiNAk")
    }

    "decode decodes base58 format" in {
      val i = BigInt("987654321987654321")
      assert(BigInt(Base58.decode("3HyFRGmiNAk")) == i)
    }

    "encode and decode are symmetric" in {
      val i = BigInt(160, util.Random)
      assert(BigInt(Base58.decode(Base58.encode(i.toByteArray))) == i)
    }
  }
