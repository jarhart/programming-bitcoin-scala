package ecc

import org.scalatest.funspec.AnyFunSpec
import scala.util.Random

class SignatureSpec extends AnyFunSpec:

  describe("der encodes in the DER format"):
    it("der encodes in the DER format"):

      val signatures = Seq(
        Signature(1, 2),
        Signature(BigInt(256, Random), BigInt(255, Random)),
        Signature(BigInt(256, Random), BigInt(255, Random))
      )

      for (sig <- signatures)
        assert(sig == Signature.parse(sig.der))
