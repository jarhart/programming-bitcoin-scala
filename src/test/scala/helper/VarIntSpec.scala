package helper

import org.scalatest.freespec.AnyFreeSpec
import java.util.HexFormat
import scala.util.Random

class VarIntSpec extends AnyFreeSpec:

  val hexFormat = HexFormat.of()
  import hexFormat.{formatHex, parseHex}

  "read" - {

    "reads a single-byte VarInt from Bytes" in {
      val i = BigInt(234)
      val extra = Random.nextBytes(9)
      val bytes = LittleEndian.fromInt(i, 1) ++ extra
      val (result, rest) = VarInt.read(LazyList(bytes*))
      assert(result == i)
      assert(rest.length == extra.length)
    }

    "reads 253 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfd00") ++ extra
      val (result, rest) = VarInt.read(LazyList(bytes*))
      assert(result == BigInt(253))
      assert(rest.length == extra.length)
    }

    "reads 254 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfe00") ++ extra
      val (result, rest) = VarInt.read(LazyList(bytes*))
      assert(result == BigInt(254))
      assert(rest.length == extra.length)
    }

    "reads 255 encoded as a VarInt" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdff00") ++ extra
      val (result, rest) = VarInt.read(LazyList(bytes*))
      assert(result == BigInt(255))
      assert(rest.length == extra.length)
    }

    "reads a larger VarInt from Bytes" in {
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fe40e20100") ++ extra
      val (result, rest) = VarInt.read(LazyList(bytes*))
      assert(result == BigInt(123456))
      assert(rest.length == extra.length)
    }
  }

  "encode" - {
    "encodes a single-byte VarInt" in {
      assert(formatHex(VarInt.encode(234)) == "ea")
    }

    "encodes 253 as a VarInt" in {
      assert(formatHex(VarInt.encode(253)) == "fdfd00")
    }

    "encodes 254 as a VarInt" in {
      assert(formatHex(VarInt.encode(254)) == "fdfe00")
    }

    "encodes 255 as a VarInt" in {
      assert(formatHex(VarInt.encode(255)) == "fdff00")
    }

    "encodes a larger VarInt" in {
      assert(formatHex(VarInt.encode(123456)) == "fe40e20100")
    }
  }
