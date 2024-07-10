package tx.script

import helper.*
import org.scalatest.funspec.AnyFunSpec

class OpsSpec extends AnyFunSpec:

  describe("op0"):
    it("pushes a 0 onto the stack"):
      Op.runS(Ops.op0)(Nil) match
        case Some(List(e)) => assert(Num.decode(e) == 0)
        case None          => fail("Op failed")

  describe("op1negate"):
    it("pushes a -1 onto the stack"):
      Op.runS(Ops.op1negate)(Nil) match
        case Some(List(e)) => assert(Num.decode(e) == -1)
        case None          => fail("Op failed")

  describe("op16"):
    it("pushes a 16 onto the stack"):
      Op.runS(Ops.op16)(Nil) match
        case Some(List(e)) => assert(Num.decode(e) == 16)
        case None          => fail("Op failed")

  describe("opDrop"):
    it("drops an item from the stack"):
      val stack = List(2, 5) map (BigInt(_).toByteArray)
      Op.runS(Ops.opDrop)(stack) match
        case Some(List(e)) => assert(Num.decode(e) == 5)
        case None          => fail("Op failed")

  describe("opDup"):
    it("duplicates the item on the top of the stack"):
      val stack = List(2, 5) map (BigInt(_).toByteArray)
      Op.runS(Ops.opDup)(stack) match
        case Some(List(a, b, _)) =>
          assert(Num.decode(a) == 2 && Num.decode(b) == 2)
        case None => fail("Op failed")

  describe("op2drop"):
    it("drops 2 items from the stack"):
      val stack = List(2, 5, 9) map (BigInt(_).toByteArray)
      Op.runS(Ops.op2drop)(stack) match
        case Some(List(e)) => assert(Num.decode(e) == 9)
        case None          => fail("Op failed")

  describe("op2dup"):
    it("duplicates 2 items on the top of the stack"):
      val stack = List(2, 5, 9) map (BigInt(_).toByteArray)
      Op.runS(Ops.op2dup)(stack) match
        case Some(List(a, b, c, d, _)) =>
          assert(
            Num.decode(a) == 2 && Num.decode(b) == 5
              && Num.decode(c) == 2 && Num.decode(d) == 5
          )
        case None => fail("Op failed")

  describe("op1add"):
    it("adds 1 to the value at the top of the stack"):
      val stack = List(BigInt(3).toByteArray)
      Op.runS(Ops.op1add)(stack) match
        case Some(List(e)) => assert(Num.decode(e) == 4)
        case None          => fail("Op failed")

  describe("opAdd"):
    it("adds 2 values at the top of the stack"):
      val stack = List(BigInt(3).toByteArray, BigInt(4).toByteArray)
      Op.runS(Ops.opAdd)(stack) match
        case Some(List(e)) => assert(Num.decode(e) == 7)
        case None          => fail("Op failed")

  describe("opHash160"):
    it("performs a hash160"):
      val stack = List("hello world".getBytes())
      Op.runS(Ops.opHash160)(stack) match
        case Some(List(e)) =>
          assert(formatHex(e) == "d7d5ee7824ff93f94c3055af9382c86c68b5ca92")
        case None => fail("Op failed")

  describe("opCheckSig"):
    it("checks a simple signature"):
      val z = BigInt(
        "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d",
        16
      )
      val sec = parseHex(
        "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34"
      )
      val sig = parseHex(
        "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601"
      )
      val stack = List(sec, sig)
      Op.runS(Ops.opCheckSig)(stack, z) match
        case Some(e :: _) => assert(Num.decode(e) == BigInt(1))
        case None         => fail("Op failed")
