package ecc

trait Coordinate[T] extends RMul[T]:

  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def mul(a: T, b: T): T
  def div(a: T, b: T): T
  def ipow(x: T, e: Int): T

  extension (a: T)
    def +(b: T): T = add(a, b)
    def -(b: T): T = sub(a, b)
    def *(b: T): T = mul(a, b)
    def /(b: T): T = div(a, b)
    def pow(e: Int): T = ipow(a, e)

given Coordinate[BigInt] with
  def add(a: BigInt, b: BigInt) = a + b
  def sub(a: BigInt, b: BigInt) = a - b
  def mul(a: BigInt, b: BigInt) = a * b
  def div(a: BigInt, b: BigInt) = a / b
  def ipow(a: BigInt, b: Int) = a pow b
  def rmul(coeff: BigInt, x: BigInt) = coeff * x
