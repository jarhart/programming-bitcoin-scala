package ecc

import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class PrivateKeySpec extends AnyFreeSpec {
  
  "PrivateKey" - {

    "sign creates valid signatures" in {
      val pk = PrivateKey.random()
      val z = BigInt(256, Random)
      val sig = pk.sign(z)
      assert(pk.point.verify(z, sig))
    }
  }
}
