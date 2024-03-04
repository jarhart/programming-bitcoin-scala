package ecc

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PointSpec extends AnyFunSpec with ScalaCheckPropertyChecks:

  describe("Point"):

    val a = BigInt(5)
    val b = BigInt(7)
    type A = a.type
    type B = b.type

    it("refuses to construct points not on the curve"):
      assertThrows[IllegalArgumentException]:
        Point[BigInt, A, B](-2, 4)

    it("constructs points on a curve"):
      Point[BigInt, A, B](3, -7)
      Point[BigInt, A, B](18, 77)

    describe("operator +"):

      it("adds points - case 0"):
        val a = Point.atInfinity[BigInt, A, B]
        val b = Point[BigInt, A, B](2, 5)
        val c = Point[BigInt, A, B](2, -5)
        assert(a + b == b)
        assert(b + a == b)
        assert(b + c == a)

      it("adds points - case 1"):
        val a = Point[BigInt, A, B](3, 7)
        val b = Point[BigInt, A, B](-1, -1)
        assert(a + b == Point[BigInt, A, B](2, -5))

      it("adds points - case 2"):
        val a = Point[BigInt, A, B](-1, -1)
        assert(a + a == Point[BigInt, A, B](18, 77))

    describe("over a finite field"):

      val p = BigInt(223)
      type P = p.type
      val a = FieldElement[P](0)
      val b = FieldElement[P](7)
      type A = a.type
      type B = b.type
      type C = FieldElement[P]

      it("constructs valid points"):
        for ((x_raw, y_raw) <- Seq((192, 105), (17, 56), (1, 193)))
          val x = FieldElement[P](x_raw)
          val y = FieldElement[P](y_raw)
          Point[C, A, B](x, y)

      it("refuses to construct invalid points"):
        for ((x_raw, y_raw) <- Seq((200, 119), (42, 99)))
          val x = FieldElement[P](x_raw)
          val y = FieldElement[P](y_raw)
          assertThrows[IllegalArgumentException] { Point[C, A, B](x, y) }

      describe("operator +"):
        it("add points"):
          val additions = Table(
            ("x1", "y1", "x2", "y2", "x3", "y3"),
            (192, 105, 17, 56, 170, 142),
            (47, 71, 117, 141, 60, 139),
            (143, 98, 76, 66, 47, 71)
          )

          forAll(additions): (x1, y1, x2, y2, x3, y3) =>
            val a = Point[C, A, B](FieldElement(x1), FieldElement(y1))
            val b = Point[C, A, B](FieldElement(x2), FieldElement(y2))
            val c = Point[C, A, B](FieldElement(x3), FieldElement(y3))
            assert(a + b == c)

      describe("rmul"):
        it("does scalar multiplication"):
          val multiplications = Table(
            ("s", "x1", "y1", "x2", "y2"),
            (2, 192, 105, 49, 71),
            (2, 143, 98, 64, 168),
            (2, 47, 71, 36, 111),
            (4, 47, 71, 194, 51),
            (8, 47, 71, 116, 55),
            (21, 47, 71, -1, -1)
          )

          forAll(multiplications): (s, x1, y1, x2, y2) =>
            val p1 = Point[C, A, B](FieldElement(x1), FieldElement(y1))
            val p2 =
              if x2 < 0 then Point.atInfinity[C, A, B]
              else Point[C, A, B](FieldElement(x2), FieldElement(y2))

            assert(s * p1 == p2)
