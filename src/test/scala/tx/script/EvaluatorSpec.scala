package tx.script

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class EvaluatorSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("Evaluator"):

    describe("run"):
      import OpCode.*

      it("runs a sequence of commands"):
        val cases = Table(
          ("commands", "expected"),
          Seq(OP_5, OP_4, OP_3, OP_MUL, OP_ADD) -> Some(BigInt(17)),
          Seq(OP_3, OP_5, OP_MUL, OP_15, OP_EQUAL) -> Some(BigInt(1))
        )

        forAll(cases): (commands, expected) =>
          assert(Evaluator.run(commands, 0) == expected)
