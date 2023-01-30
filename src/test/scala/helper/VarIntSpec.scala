package helper

import org.scalatest.freespec.AnyFreeSpec
import java.util.HexFormat
import scala.util.Random

class VarIntSpec extends AnyFreeSpec:

  val hexFormat = HexFormat.of()
  import hexFormat.{formatHex, parseHex}

  "parse" - {

    "parses a single-byte VarInt from Bytes" in {
      val i = BigInt(234)
      val extra = Random.nextBytes(9)
      val bytes = LittleEndian.fromInt(i, 1) ++ extra
      assert(VarInt.parse(bytes) == i)
    }

    "parses 253 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfd00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(253))
    }

    "parses 254 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfe00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(254))
    }

    "parses 255 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdff00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(255))
    }

    "parses a larger VarInt from Bytes" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fe40e20100") ++ extra
      assert(VarInt.parse(bytes) == BigInt(123456))
    }
  }

  "serialize" - {
    "serializes a single-byte VarInt" in {
      assert(formatHex(VarInt.serialize(234)) == "ea")
    }

    "serializes 253 as a VarInt" in {
      assert(formatHex(VarInt.serialize(253)) == "fdfd00")
    }

    "serializes 254 as a VarInt" in {
      assert(formatHex(VarInt.serialize(254)) == "fdfe00")
    }

    "serializes 255 as a VarInt" in {
      assert(formatHex(VarInt.serialize(255)) == "fdff00")
    }

    "serializes a larger VarInt" in {
      assert(formatHex(VarInt.serialize(123456)) == "fe40e20100")
    }
  }
