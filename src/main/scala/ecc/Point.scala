package ecc

import scala.annotation.tailrec

sealed trait Point[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf]:

  def a: C = valueOf[A]
  def b: C = valueOf[B]

  def isAtInfinity: Boolean

  def +(that: Point[C, A, B]): Point[C, A, B]

  def rmul(s: BigInt): Point[C, A, B]

object Point:
  def atInfinity[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf]: Point[C, A, B] =
    PointAtInfinity()

  def apply[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf](x: C, y: C): Point[C, A, B] =
    NonZeroPoint(x, y)

  given pointRMul[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf]: RMul[Point[C, A, B]] with
    def rmul(coeff: BigInt, p: Point[C, A, B]) = p.rmul(coeff)


final case class PointAtInfinity[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf]() extends Point[C, A, B]:
  override def isAtInfinity = true
  override def +(that: Point[C, A, B]) = that
  override def rmul(s: BigInt) = this


final case class NonZeroPoint[C : Coordinate, A <: C : ValueOf, B <: C : ValueOf](x: C, y: C) extends Point[C, A, B]:
  require(y.pow(2) == x.pow(3) + a * x + b, s"(${x}, ${y}) is not on the curve")
  override def isAtInfinity = false

  override def +(that: Point[C, A, B]) = that match
    case PointAtInfinity() => this

    case NonZeroPoint(`x`, `y`) if y == 0 => Point.atInfinity
  
    case NonZeroPoint(`x`, `y`) =>
      val s = (3 * x * x + a) / (2 * y)
      val x3 = s * s - 2 * x
      val y3 = s * (x - x3) - y
      Point(x3, y3)

    case NonZeroPoint(`x`, _) => Point.atInfinity

    case NonZeroPoint(x2, y2) =>
      val s = (y2 - y) / (x2 - x)
      val x3 = s * s - x - x2
      val y3 = s * (x - x3) - y
      Point(x3, y3)

  override def rmul(s: BigInt) = LazyList
    .unfold((s, this: Point[C, A, B])) { case (i, c) => Option.when(i > 0) { ((i & 1, c), (i >> 1, c + c)) } }
    .collect { case (1, c) => c }
    .foldRight(Point.atInfinity[C, A, B])(_ + _)
