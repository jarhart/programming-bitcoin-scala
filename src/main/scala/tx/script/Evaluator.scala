package tx.script

import cats.syntax.flatMap._

type Stack = List[Elem]

type Cmd = OpCode | Elem

type Elem = Array[Byte]

final case class Evaluator(stack: Stack, altStack: Stack, cmds: Seq[Cmd]):
  def run(z: BigInt): Option[Elem] = Evaluator.eval run z runA this

object Evaluator:
  def apply(cmds: Seq[Cmd]): Evaluator = this(List(), List(), cmds)

  def run(cmds: Seq[Cmd], z: BigInt): Option[Elem] = Evaluator(cmds).run(z)

  def eval: Op[Elem] = Op.next >>= (_ map (_ >> eval) getOrElse Op.head)
