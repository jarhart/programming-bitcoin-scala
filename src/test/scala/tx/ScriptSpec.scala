package tx

import org.scalatest.freespec.AnyFreeSpec

import script._

import java.util.HexFormat

class ScriptSpec extends AnyFreeSpec:
  val hexFormat = HexFormat.of()
  import hexFormat.parseHex

  val scriptPubKey = parseHex("6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937")

  "parse parses a ScriptPubKey" in {
    val script = Script.parse(LazyList from scriptPubKey)

    val expected0 = parseHex("304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601")
    script.cmds(0) match
      case actual0: Elem => assert(actual0.toSeq == expected0.toSeq)
      case _ => fail("Not an Elem")

    val expected1 = parseHex("035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937")
    script.cmds(1) match
      case actual1: Elem => assert(actual1.toSeq == expected1.toSeq)
      case _ => fail("Not an Elem")
  }

  "serializes to a ScriptPubKey" in {
    val script = Script.parse(LazyList from scriptPubKey)

    assert(script.serialize.toSeq == scriptPubKey.toSeq)
  }
