package helper

object Parser:

  def pure[A](a: A) = Parser[A]((a, _))

  val get = Parser(bytes => (bytes, bytes))

  val head = Parser(bytes => (bytes.head, bytes.tail))

  def take(n: Int): Parser[LazyList[Byte]] = Parser(_ splitAt n)

  def takeBytes(n: Int): Parser[Array[Byte]] = take(n) map (_.toArray)

  def reverseBytes(n: Int): Parser[Array[Byte]] = takeBytes(n) map (_.reverse)

  def unsigned(n: Int): Parser[BigInt] = takeBytes(n) map unsignedFromBytes

  def times[A](n: BigInt)(parser: Parser[A]): Parser[Seq[A]] =
    (BigInt(1) to n foldLeft pure(Seq()))((pAcc, _) =>
      for (acc <- pAcc; a <- parser) yield acc :+ a)

  def many[A](length: Int)(parser: Parser[A]): Parser[Seq[A]] =
    take(length).map(LazyList.unfold(_)(bytes =>
      Option.when(bytes.nonEmpty)(parser.run(bytes))))


class Parser[A](val run: LazyList[Byte] => (A, LazyList[Byte])) extends (LazyList[Byte] => A):

  override def apply(input: LazyList[Byte]) =
    run(input) match
      case (value, _) => value

  def apply(input: Array[Byte]): A = apply(LazyList from input)

  def map[B](f: A => B) = Parser[B](
    run(_) match
      case (a, rest) => (f(a), rest))

  def flatMap[B](f: A => Parser[B]) = Parser[B](
    run(_) match
      case (a, rest) => f(a).run(rest))

  def times(n: BigInt): Parser[Seq[A]] = Parser.times(n)(this)

  def many(length: Int): Parser[Seq[A]] = Parser.many(length)(this)
