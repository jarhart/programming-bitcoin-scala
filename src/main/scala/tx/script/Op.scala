package tx.script

import cats.data.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*

import ecc.*
import helper.*
import collection.mutable
import math.*

type Op[B] = ReaderT[[A] =>> StateT[Option, Evaluator, A], BigInt, B]

object Op:
  export OpCode.*
  export Ops.*

  def apply[A](f: BigInt => Evaluator => Option[(Evaluator, A)]): Op[A] =
    ReaderT(z => StateT(f(z)))

  def apply(cmd: Cmd): Op[Unit] = cmd match
    case opCode: OpCode => byOpCode.getOrElse(opCode, fail)
    case elem: Elem     => push(elem)

  def runS[A](op: Op[A])(stack: Stack, z: BigInt = 0): Option[Stack] =
    op.run(z).runS(Evaluator(stack, Nil, Seq.empty[Cmd])) map (_.stack)

  def pure[A](x: A): Op[A] = ReaderT.pure(x)

  def fail[A] = Op[A](_ => _ => None)

  val ask: Op[BigInt] = ReaderT.ask

  val next: Op[Option[Op[Unit]]] = liftC:
    case Seq(cmd, cmds*) => (cmds, Some(Op(cmd)))
    case cmds            => (cmds, None)

  def push(elem: Elem) = liftU:
    case stack => elem :: stack

  def push(num: BigInt): Op[Unit] = push(Num.encode(num))

  def push(i: Int): Op[Unit] = push(BigInt(i))

  def push(b: Boolean): Op[Unit] = push(if b then 1 else 0)

  def push(elems: List[Elem]) = liftU: stack =>
    Some(elems ++ stack)

  val peek: Op[Elem] = lift[Elem]:
    case stack @ (a :: _) => (stack, a)

  def peek(n: Int) = lift[List[Elem]]: stack =>
    Option.when(stack.size >= n):
      (stack, stack.take(n))

  def peekAt(n: Int) = lift[Elem]: stack =>
    Option.when(stack.isDefinedAt(n)):
      (stack, stack(n))

  val pop = lift[Elem]:
    case a :: stack => (stack, a)

  def pop(n: Int) = lift[List[Elem]]: stack =>
    Option.when(stack.size >= n):
      stack.splitAt(n) match
        case (r, s) => (s, r)

  def popAt(n: Int) = lift[Elem]: stack =>
    Option.when(stack.size >= n):
      stack.splitAt(n) match
        case (xs, ys) => (xs ++ ys.tail, ys.head)

  def popMap[A](f: Elem => A): Op[A] = pop map f

  def popMap[A](n: Int)(f: Elem => A): Op[List[A]] =
    pop(n) map (_ map f)

  def popMapOpt[A](f: Elem => Option[A]) = lift[A]:
    _ match
      case a :: stack => f(a) map ((stack, _))
      case _          => None

  def popMapOpt[A](n: Int)(f: Elem => Option[A]) = lift[List[A]]: stack =>
    Option
      .when(stack.size >= n):
        stack splitAt n match
          case (r, s) => (s, r)
      .flatMap: (s, elems) =>
        elems traverse f map:
          (s, _)

  val popBigInt: Op[BigInt] = popMap(Num.decode)

  val popInt: Op[Int] = popBigInt map (_.toInt)

  val popS256Point: Op[S256Point] = popMapOpt(S256Point.parse)

  def popS256Points(n: Int): Op[List[S256Point]] = popMapOpt(n)(S256Point.parse)

  val popSignature: Op[Signature] = popMap(s => Signature.parse(s dropRight 1))

  def popSignatures(n: Int): Op[List[Signature]] =
    popMap(n)(s => Signature.parse(s dropRight 1))

  val altPop = liftS[Elem]:
    case a :: stack => (stack, a)

  def altPush(elem: Elem) = liftS[Unit]:
    case stack => (elem :: stack, ())

  val result: Op[BigInt] = pop map Num.decode

  val stackDepth: Op[Int] = lift[Int]: stack =>
    Some(stack, stack.length)

  val popCmd: Op[Cmd] = liftC:
    case Seq(cmd, cmds*) => (cmds, cmd)

  def pushCmd(cmd: Cmd): Op[Unit] = liftC:
    cmds => Some(cmd +: cmds, ())

  def pushBranch(branch: Seq[Cmd]): Op[Unit] = liftC:
    cmds => Some(branch ++ cmds, ())

  def unary(f: Elem => Elem) = for
    e <- pop
    _ <- push(f(e))
  yield ()

  def intUnary(f: BigInt => BigInt) = unary(a => Num.encode(f(Num.decode(a))))

  def binary(f: (Elem, Elem) => Elem) = for
    List(a, b) <- pop(2)
    _ <- push(f(a, b))
  yield ()

  def intBinary(f: (BigInt, BigInt) => BigInt) =
    binary((x, y) => Num.encode(f(Num.decode(x), Num.decode(y))))

  def trinary(f: (Elem, Elem, Elem) => Elem) = for
    List(a, b, c) <- pop(3)
    _ <- push(f(a, b, c))
  yield ()

  def intTrinary(f: (BigInt, BigInt, BigInt) => BigInt) =
    trinary: (x, y, z) =>
      Num.encode(f(Num.decode(x), Num.decode(y), Num.decode(z)))

  private def lift[A](f: Stack => Option[(Stack, A)]) = liftM:
    case m @ Evaluator(stack, _, _) =>
      f(stack) map { case (s, x) => (m.copy(stack = s), x) }

  private def lift[A](pf: PartialFunction[Stack, (Stack, A)]): Op[A] =
    lift(pf.lift)

  private def liftS[A](f: Stack => Option[(Stack, A)]) = liftM:
    case m @ Evaluator(_, stack, _) =>
      f(stack) map { case (s, x) => (m.copy(stack = s), x) }

  private def liftS[A](pf: PartialFunction[Stack, (Stack, A)]): Op[A] =
    liftS(pf.lift)

  private def liftU(f: Stack => Option[Stack]) = liftM:
    case m @ Evaluator(stack, _, _) =>
      f(stack) map (s => (m.copy(stack = s), ()))

  private def liftU(pf: PartialFunction[Stack, Stack]): Op[Unit] =
    liftU(pf.lift)

  private def liftC[A](f: (Seq[Cmd]) => Option[(Seq[Cmd], A)]) = liftM:
    case m @ Evaluator(_, _, cmds) =>
      f(cmds) map:
        case (c, x) => (m.copy(cmds = c), x)

  private def liftC[A](pf: PartialFunction[(Seq[Cmd]), (Seq[Cmd], A)]): Op[A] =
    liftC(pf.lift)

  private def liftM[A](f: Evaluator => Option[(Evaluator, A)]) =
    Op[A](Function.const(f))

  private def liftM[A](pf: PartialFunction[Evaluator, (Evaluator, A)]): Op[A] =
    liftM(pf.lift)
