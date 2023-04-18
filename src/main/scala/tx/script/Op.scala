package tx.script

import cats.data._
import cats.syntax.flatMap._

import ecc._
import helper._
import collection.mutable
import math._

type Op[B] = ReaderT[[A] =>> StateT[Option, Evaluator, A], BigInt, B]

object Op:
  export OpCode._
  export Ops._

  def apply[A](f: BigInt => Evaluator => Option[(Evaluator, A)]): Op[A] =
    ReaderT(z => StateT(f(z)))

  def apply(cmd: Cmd): Op[Unit] = cmd match {
    case opCode: OpCode => byOpCode.getOrElse(opCode, fail)
    case elem: Elem => push(elem)
  }

  def pure[A](x: A): Op[A] = ReaderT.pure(x)

  def fail[A] = Op[A](_ => _ => None)

  val head: Op[Elem] = lift(stack => stack.headOption map ((stack.tail, _)))

  val result: Op[BigInt] = head map Num.decode

  val next: Op[Option[Op[Unit]]] = liftC {
    case (stack, Seq(cmd, cmds*)) => (stack, cmds, Some(Op(cmd)))
    case (stack, cmds) => (stack, cmds, None)
  }

  def push(elem: Elem) = liftU { case stack => elem :: stack }

  def push(num: BigInt): Op[Unit] = push(Num.encode(num))

  def unary(f: Elem => Elem) = liftU { case a :: stack => f(a) :: stack }

  def intUnary(f: BigInt => BigInt) = unary(a => Num.encode(f(Num.decode(a))))

  def binary(f: (Elem, Elem) => Elem) = liftU {
    case a :: b :: stack => f(a, b) :: stack
  }

  def intBinary(f: (BigInt, BigInt) => BigInt) =
    binary((x, y) => Num.encode(f(Num.decode(x), Num.decode(y))))

  def trinary(f: (Elem, Elem, Elem) => Elem) = liftU {
    case a :: b :: c :: stack => f(a, b, c) :: stack
  }

  def intTrinary(f: (BigInt, BigInt, BigInt) => BigInt) =
    trinary((x, y, z) => Num.encode(f(Num.decode(x), Num.decode(y), Num.decode(z))))

  def lift[A](f: Stack => Option[(Stack, A)]) = liftM {
    case m @ Evaluator(stack, _, _) =>
      f(stack) map { case (s, x) => (m.copy(stack = s), x) }
  }

  def lift[A](pf: PartialFunction[Stack, (Stack, A)]): Op[A] = lift(pf.lift)

  def liftU(f: Stack => Option[Stack]) = liftM {
    case m @ Evaluator(stack, _, _) => f(stack) map (s => (m.copy(stack = s), ()))
  }

  def liftU(pf: PartialFunction[Stack, Stack]): Op[Unit] = liftU(pf.lift)

  def liftC[A](f: (Stack, Seq[Cmd]) => Option[(Stack, Seq[Cmd], A)]) = liftM {
    case m @ Evaluator(stack, _, cmds) =>
      f(stack, cmds) map { case (s, c, x) => (m.copy(stack = s, cmds = c), x) }
  }

  def liftC[A](pf: PartialFunction[(Stack, Seq[Cmd]), (Stack, Seq[Cmd], A)]): Op[A] =
    liftC { (stack, cmds) =>
      Option.when(pf.isDefinedAt((stack, cmds)))(pf((stack, cmds)))
    }

  def liftCU(f: (Stack, Seq[Cmd]) => Option[(Stack, Seq[Cmd])]) = liftM {
    case m @ Evaluator(stack, _, cmds) =>
      f(stack, cmds) map { case (s, c) => (m.copy(stack = s, cmds = c), ()) }
  }

  def liftCU(pf: PartialFunction[(Stack, Seq[Cmd]), (Stack, Seq[Cmd])]): Op[Unit] =
    liftCU { (stack, cmds) =>
      Option.when(pf.isDefinedAt((stack, cmds)))(pf((stack, cmds)))
    }

  def liftSU(f: (Stack, Stack) => Option[(Stack, Stack)]) = liftM {
    case m @ Evaluator(stack, altStack, _) =>
      f(stack, altStack) map { case (s, a) => (m.copy(stack = s, altStack = a), ()) }
  }

  def liftSU(pf: PartialFunction[(Stack, Stack), (Stack, Stack)]): Op[Unit] =
    liftSU { (stack, altStack) =>
      Option.when(pf.isDefinedAt((stack, altStack)))(pf((stack, altStack)))
    }

  def liftZU(f: (Stack, BigInt) => Option[Stack]) = Op[Unit](z => {
    case m @ Evaluator(stack, _, _) => f(stack, z) map (s => (m.copy(stack = s), ()))
  })

  def liftZU(pf: PartialFunction[(Stack, BigInt), Stack]): Op[Unit] = liftZU { (stack, z) =>
    Option.when(pf.isDefinedAt((stack, z)))(pf(stack, z))
  }

  def liftM[A](f: Evaluator => Option[(Evaluator, A)]) = Op[A](Function.const(f))

  def liftM[A](pf: PartialFunction[Evaluator, (Evaluator, A)]): Op[A] = liftM(pf.lift)
