package ecc

import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class PrivateKeySpec extends AnyFreeSpec:
  
  "PrivateKey" - {

    "sign creates valid signatures" in {
      val pk = PrivateKey.random()
      val z = BigInt(256, Random)
      val sig = pk.sign(z)
      assert(pk.point.verify(z, sig))
    }

    "wif returns the WIF format" in {
      val keys = Seq(
        (PrivateKey(BigInt(2).pow(256) - BigInt(2).pow(199)), true, false, "L5oLkpV3aqBJ4BgssVAsax1iRa77G5CVYnv9adQ6Z87te7TyUdSC"),
        (PrivateKey(BigInt(2).pow(256) - BigInt(2).pow(201)), false, true, "93XfLeifX7Jx7n7ELGMAf1SUR6f9kgQs8Xke8WStMwUtrDucMzn"),
        (PrivateKey(BigInt("0dba685b4511dbd3d368e5c4358a1277de9486447af7b3604a69b8d9d8b7889d", 16)), false, false, "5HvLFPDVgFZRK9cd4C5jcWki5Skz6fmKqi1GQJf5ZoMofid2Dty"),
        (PrivateKey(BigInt("1cca23de92fd1862fb5b76e5f4f50eb082165e5191e116c18ed1a6b24be6a53f", 16)), true, true, "cNYfWuhDpbNM1JWc3c6JTrtrFVxU4AGhUKgw5f93NP2QaBqmxKkg"),
      )

      for ((pk, compressed, testnet, expected) <- keys)
        assert(pk.wif(compressed=compressed, testnet=testnet) == expected)
    }
  }
