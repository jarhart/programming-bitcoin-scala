package ecc

import org.scalatest.freespec.AnyFreeSpec

class PointSpec extends AnyFreeSpec:

  "Point" - {

    "operator +" - {

      "adds points, case 0" in {
        val a = Point.atInfinity[5, 7]
        val b = Point[5, 7](2, 5)
        val c = Point[5, 7](2, -5)
        assert(a + b == b)
        assert(b + a == b)
        assert(b + c == a)
      }

      "adds points, case 1" in {
        val a = Point[5, 7](3, 7)
        val b = Point[5, 7](-1, -1)
        assert(a + b == Point[5, 7](2, -5))
      }

      "adds points, case 2" in {
        val a = Point[5, 7](-1, -1)
        assert(a + a == Point[5, 7](18, 77))
      }
    }
  }
