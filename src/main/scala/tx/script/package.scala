package tx.script

extension [T](stack: List[T])

  def pop(): Option[(T, List[T])] = stack match
    case h :: t => Some((h, t))
    case _      => None

  infix def pop(n: Int): Option[(List[T], List[T])] =
    Option.when(stack.size >= n):
      stack.splitAt(n)

  infix def popWith[U](f: T => U): Option[(U, List[T])] =
    for (h, t) <- pop()
    yield (f(h), t)

  infix def flatPopWith[U](f: T => Option[U]): Option[(U, List[T])] =
    for
      (h, t) <- pop()
      u <- f(h)
    yield (u, t)

  def popWith[U](n: Int)(f: List[T] => U): Option[(U, List[T])] =
    for (l, r) <- pop(n)
    yield (f(l), r)
