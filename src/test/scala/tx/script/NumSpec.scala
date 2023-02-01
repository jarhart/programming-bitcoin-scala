package tx.script

import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import java.util.HexFormat

class NumSpec extends AnyFreeSpec with ScalaCheckPropertyChecks:

  val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  "Num.encode" - {
    "encodes 0 as no bytes" in {
      assert(Num.encode(0).isEmpty)
    }

    "encodes positive numbers" in {
      assert(formatHex(Num.encode(234)) == "ea00")
      assert(formatHex(Num.encode(34567)) == "078700")
    }

    "encodes negative numbers" in {
      assert(formatHex(Num.encode(-234)) == "ea80")
      assert(formatHex(Num.encode(-34567)) == "078780")
    }
  }

  "Num.decode is the inverse of Num.encode" in {
    forAll { (i: Int) =>
      val num = BigInt(i)
      assert(Num.decode(Num.encode(num)) == num)
    }
  }
