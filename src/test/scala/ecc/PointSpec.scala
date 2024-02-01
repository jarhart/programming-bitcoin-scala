package ecc

import org.scalatest.freespec.AnyFreeSpec

class PointSpec extends AnyFreeSpec:

  "Point" - {
  
    val a = BigInt(5)
    val b = BigInt(7)
    type A = a.type
    type B = b.type
  
    "refuses to construct points not on the curve" in:
      assertThrows[IllegalArgumentException]:
        Point[BigInt, A, B](-2, 4)
  
    "constructs points on a curve" in:
      Point[BigInt, A, B](3, -7)
      Point[BigInt, A, B](18, 77)

    "operator + adds points" - {

      "case 0" in:
        val a = Point.atInfinity[BigInt, A, B]
        val b = Point[BigInt, A, B](2, 5)
        val c = Point[BigInt, A, B](2, -5)
        assert(a + b == b)
        assert(b + a == b)
        assert(b + c == a)

      "case 1" in:
        val a = Point[BigInt, A, B](3, 7)
        val b = Point[BigInt, A, B](-1, -1)
        assert(a + b == Point[BigInt, A, B](2, -5))

      "case 2" in:
        val a = Point[BigInt, A, B](-1, -1)
        assert(a + a == Point[BigInt, A, B](18, 77))
    }

    "over a finite field" - {

      val p = BigInt(223)
      type P = p.type
      val a = FieldElement[P](0)
      val b = FieldElement[P](7)
      type A = a.type
      type B = b.type
      type C = FieldElement[P]
    
      "constructs valid points" in:
        for ((x_raw, y_raw) <- Seq((192, 105), (17, 56), (1, 193)))
          val x = FieldElement[P](x_raw)
          val y = FieldElement[P](y_raw)
          Point[C, A, B](x, y)
  
      "refuses to construct invalid points" in:
        for ((x_raw, y_raw) <- Seq((200, 119), (42, 99)))
          val x = FieldElement[P](x_raw)
          val y = FieldElement[P](y_raw)
          assertThrows[IllegalArgumentException] { Point[C, A, B](x, y) }

      "operator + add points" in:
        val additions = Seq(
          (192, 105, 17, 56, 170, 142),
          (47, 71, 117, 141, 60, 139),
          (143, 98, 76, 66, 47, 71),
        )

        for ((x1_raw, y1_raw, x2_raw, y2_raw, x3_raw, y3_raw) <- additions)
          val a = Point[C, A, B](FieldElement(x1_raw), FieldElement(y1_raw))
          val b = Point[C, A, B](FieldElement(x2_raw), FieldElement(y2_raw))
          val c = Point[C, A, B](FieldElement(x3_raw), FieldElement(y3_raw))
          assert(a + b == c)

      "rmul does scalar multiplication" in:
        val multiplications = Seq(
          (2, 192, 105, 49, 71),
          (2, 143, 98, 64, 168),
          (2, 47, 71, 36, 111),
          (4, 47, 71, 194, 51),
          (8, 47, 71, 116, 55),
          (21, 47, 71, -1, -1),
        )
        
        for ((s, x1_raw, y1_raw, x2_raw, y2_raw) <- multiplications)
          val p1 = Point[C, A, B](FieldElement(x1_raw), FieldElement(y1_raw))
          val p2 =
            if x2_raw < 0 then
              Point.atInfinity[C, A, B]
            else
              Point[C, A, B](FieldElement(x2_raw), FieldElement(y2_raw))

          assert(s * p1 == p2)
    }
  }