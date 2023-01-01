package ecc

sealed trait Point[A <: Int, B <: Int](using evA: ValueOf[A], evB: ValueOf[B]):

  def a: Int = valueOf[A]
  def b: Int = valueOf[B]

  def isAtInfinity: Boolean

  def +(that: Point[A, B]): Point[A, B]

object Point:
  def atInfinity[A <: Int, B <: Int](using evA: ValueOf[A], evB: ValueOf[B]): Point[A, B] =
    PointAtInfinity()

  def apply[A <: Int, B <: Int](x: Int, y: Int)(using evA: ValueOf[A], evB: ValueOf[B]): Point[A, B] =
    NonZeroPoint(x, y)

final case class PointAtInfinity[A <: Int, B <: Int]()(using evA: ValueOf[A], evB: ValueOf[B]) extends Point[A, B]:
  override def isAtInfinity = true
  override def +(that: Point[A, B]) = that

final case class NonZeroPoint[A <: Int, B <: Int](x: Int, y: Int)(using evA: ValueOf[A], evB: ValueOf[B]) extends Point[A, B]:
  assert(pow(y, 2) == pow(x, 3) + a * x + b, s"(${x}, ${y}) is not on the curve")
  override def isAtInfinity = false

  override def +(that: Point[A, B]) = that match {

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
  }
