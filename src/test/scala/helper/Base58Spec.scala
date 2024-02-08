package helper

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
        val addr = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
        val expected = "507b27411ccf7f16f10297de6cef3f291623eddf"
        assert(formatHex(Base58.decode(addr)) == expected)

  describe("Base58check"):

    describe("encode"):
      it("encodes in base58check format"):
        val hex = "6f507b27411ccf7f16f10297de6cef3f291623eddf"
        val expected = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
        assert(Base58check.encode(parseHex(hex)) == expected)
