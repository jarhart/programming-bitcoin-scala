package ecc

import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class SignatureSpec extends AnyFreeSpec:

  "der encodes in the DER format" in:

    val signatures = Seq(
      Signature(1, 2),
      Signature(BigInt(256, Random), BigInt(255, Random)),
      Signature(BigInt(256, Random), BigInt(255, Random)),
    )

    for (sig <- signatures)
      assert(sig == Signature.parse(sig.der))
