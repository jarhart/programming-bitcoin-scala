package tx

import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import java.util.HexFormat

class OpSpec extends AnyFreeSpec with ScalaCheckPropertyChecks:

  import Op.{decodeNum, encodeNum}
  val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  "encodeNum" - {
    "encodes 0 as no bytes" in {
      assert(encodeNum(0).isEmpty)
    }

    "encodes positive numbers" in {
      assert(formatHex(encodeNum(234)) == "ea00")
      assert(formatHex(encodeNum(34567)) == "078700")
    }

    "encodes negative numbers" in {
      assert(formatHex(encodeNum(-234)) == "ea80")
      assert(formatHex(encodeNum(-34567)) == "078780")
    }
  }

  "decodeNum is the inverse of encodeNum" in {
    forAll { (i: Int) =>
      val num = BigInt(i)
      assert(decodeNum(encodeNum(num)) == num)
    }
  }
