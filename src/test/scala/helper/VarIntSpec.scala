package helper

import helper.*
import org.scalatest.funspec.AnyFunSpec

import scala.util.Random

class VarIntSpec extends AnyFunSpec:

  describe("parse"):

    it("parses a single-byte VarInt from Bytes"):
      val i = BigInt(234)
      val extra = Random.nextBytes(9)
      val bytes = LittleEndian.fromInt(i, 1) ++ extra
      assert(VarInt.parse(bytes) == i)

    it("parses 253 encoded as a VarInt"):
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfd00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(253))

    it("parses 254 encoded as a VarInt"):
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdfe00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(254))

    it("parses 255 encoded as a VarInt"):
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fdff00") ++ extra
      assert(VarInt.parse(bytes) == BigInt(255))

    it("parses a larger VarInt from Bytes"):
      val extra = Random.nextBytes(9)
      val bytes = parseHex("fe40e20100") ++ extra
      assert(VarInt.parse(bytes) == BigInt(123456))

  describe("serialize"):

    it("serializes a single-byte VarInt"):
      assert(formatHex(VarInt.serialize(234)) == "ea")

    it("serializes 253 as a VarInt"):
      assert(formatHex(VarInt.serialize(253)) == "fdfd00")

    it("serializes 254 as a VarInt"):
      assert(formatHex(VarInt.serialize(254)) == "fdfe00")

    it("serializes 255 as a VarInt"):
      assert(formatHex(VarInt.serialize(255)) == "fdff00")

    it("serializes a larger VarInt"):
      assert(formatHex(VarInt.serialize(123456)) == "fe40e20100")
