package ecc

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

class SignatureSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("der encodes in the DER format"):
    it("der encodes in the DER format"):

      val randomSignatures = Seq.fill(10):
        Signature(BigInt(256, Random), BigInt(255, Random))

      val signatures = Table("sig", (Signature(1, 2) +: randomSignatures)*)

      forAll(signatures): sig =>
        assert(sig == Signature.parse(sig.der))
