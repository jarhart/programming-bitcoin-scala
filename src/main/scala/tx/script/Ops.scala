package tx.script

import ecc.*
import helper.*
import collection.mutable
import math.*

import cats.syntax.flatMap.*

object Ops extends PopBranches:
  import Op.*

  val op0 = push(0)
  val op1negate = push(-1)
  val op1 = push(1)
  val op2 = push(2)
  val op3 = push(3)
  val op4 = push(4)
  val op5 = push(5)
  val op6 = push(6)
  val op7 = push(7)
  val op8 = push(8)
  val op9 = push(9)
  val op10 = push(10)
  val op11 = push(11)
  val op12 = push(12)
  val op13 = push(13)
  val op14 = push(14)
  val op15 = push(15)
  val op16 = push(16)

  val opNop = pure(())

  val opIf = for
    i <- popBigInt
    (trueBranch, falseBranch) <- popBranches
    _ <- pushBranch(if i == 0 then falseBranch else trueBranch)
  yield ()

  val opNotIf = for
    i <- popBigInt
    (trueBranch, falseBranch) <- popBranches
    _ <- pushBranch(if i == 0 then trueBranch else falseBranch)
  yield ()

  val opVerify = for
    i <- popBigInt
    _ <- if i == 0 then fail else pure(())
  yield ()

  val opReturn = fail[Unit]

  val opToAltStack = pop >>= altPush

  val opFromAltStack = altPop >>= push

  val op2drop = for
    _ <- pop
    _ <- pop
  yield ()

  val op2dup = peek(2) >>= push

  val op3dup = peek(3) >>= push

  val op2over = for
    List(_, _, a, b) <- peek(4)
    _ <- push(List(a, b))
  yield ()

  val op2rot = for
    List(a, b, c, d, e, f) <- pop(6)
    _ <- push(List(e, f, a, b, c, d))
  yield ()

  val op2swap = for
    List(a, b, c, d) <- pop(4)
    _ <- push(List(c, d, a, b))
  yield ()

  val opIfDup = for
    n <- popBigInt
    _ <- if n == 0 then pure(()) else push(n)
  yield ()

  val opDepth = stackDepth >>= push

  val opDrop = pop map (_ => ())

  val opDup = peek >>= push

  val opNip = for
    List(e, _) <- pop(2)
    _ <- push(e)
  yield ()

  val opOver = for
    List(_, e, _) <- pop(3)
    _ <- push(e)
  yield ()

  val opPick = popInt >>= peekAt >>= push

  val opRoll = popInt >>= popAt >>= push

  val opRot = for
    List(a, b, c) <- pop(3)
    _ <- push(List(c, a, b))
  yield ()

  val opSwap = pop(2) map (_.reverse) >>= push

  val opTuck = for
    a <- pop
    b <- peek
    _ <- push(List(a, b))
  yield ()

  val opSize = for
    e <- peek
    _ <- push(e.length)
  yield ()

  val opEqual = for
    List(a, b) <- pop(2)
    _ <- push(if a.toSeq == b.toSeq then 1 else 0)
  yield ()

  val opEqualVerify = opEqual >> opVerify

  val op1add = intUnary(_ + 1)

  val op1sub = intUnary(_ - 1)

  val opNegate = intUnary(-_)

  val opAbs = intUnary(_.abs)

  val opNot = intUnary(i => if i == 0 then 1 else 0)

  val op0notEqual = intUnary(i => if i == 0 then 0 else 1)

  val opAdd = intBinary(_ + _)

  val opSub = intBinary(_ - _)

  val opMul = intBinary(_ * _)

  val opBoolAnd = intBinary((i, j) => if i != 0 && j != 0 then 1 else 0)

  val opBoolOr = intBinary((i, j) => if i != 0 || j != 0 then 1 else 0)

  val opNumEqual = intBinary((i, j) => if i == j then 1 else 0)

  val opNumEqualVerify = opNumEqual >> opVerify

  val opNumNotEqual = intBinary((i, j) => if i == j then 0 else 1)

  val opLessThan = intBinary((i, j) => if i < j then 1 else 0)

  val opGreaterThan = intBinary((i, j) => if i > j then 1 else 0)

  val opLessThanOrEqual = intBinary((i, j) => if i <= j then 1 else 0)

  val opGreaterThanOrEqual = intBinary((i, j) => if i >= j then 1 else 0)

  val opMin = intBinary(_ min _)

  val opMax = intBinary(_ max _)

  val opWithin = intTrinary: (maximum, minimum, element) =>
    if minimum to maximum contains element then 1 else 0

  val opRipeMD160 = unary(ripemd160)

  val opSha1 = unary(sha1)

  val opSha256 = unary(sha256)

  val opHash160 = unary(hash160)

  val opHash256 = unary(hash256)

  val opCheckSig = for
    z <- ask
    pk <- popS256Point
    sig <- popSignature
    _ <- push(S256Point.verify(pk, z, sig))
  yield ()

  val opCheckSigVerify = opCheckSig >> opVerify

  val opCheckMultisig = for
    z <- ask
    n <- popInt
    pks <- popS256Points(n)
    m <- popInt
    sigs <- popSignatures(n)
    _ <- pop
    _ <- push(pks forall (pk => sigs exists (S256Point.verify(pk, z, _))))
  yield ()

  val opCheckMultisigVerify = opCheckMultisig >> opVerify

  val opCheckLocktimeVerify = Op[Unit](_ => ???)

  val opCheckSequenceVerify = Op[Unit](_ => ???)

  val byOpCode = Map[OpCode, Op[Unit]](
    OP_0 -> op0,
    OP_1NEGATE -> op1negate,
    OP_1 -> op1,
    OP_2 -> op2,
    OP_3 -> op3,
    OP_4 -> op4,
    OP_5 -> op5,
    OP_6 -> op6,
    OP_7 -> op7,
    OP_8 -> op8,
    OP_9 -> op9,
    OP_10 -> op10,
    OP_11 -> op11,
    OP_12 -> op12,
    OP_13 -> op13,
    OP_14 -> op14,
    OP_15 -> op15,
    OP_16 -> op16,
    OP_NOP -> opNop,
    OP_IF -> opIf,
    OP_NOTIF -> opNotIf,
    OP_VERIFY -> opVerify,
    OP_RETURN -> opReturn,
    OP_TOALTSTACK -> opToAltStack,
    OP_FROMALTSTACK -> opFromAltStack,
    OP_2DROP -> op2drop,
    OP_2DUP -> op2dup,
    OP_3DUP -> op3dup,
    OP_2OVER -> op2over,
    OP_2ROT -> op2rot,
    OP_2SWAP -> op2swap,
    OP_IFDUP -> opIfDup,
    OP_DEPTH -> opDepth,
    OP_DROP -> opDrop,
    OP_DUP -> opDup,
    OP_NIP -> opNip,
    OP_OVER -> opOver,
    OP_PICK -> opPick,
    OP_ROLL -> opRoll,
    OP_ROT -> opRot,
    OP_SWAP -> opSwap,
    OP_TUCK -> opTuck,
    OP_SIZE -> opSize,
    OP_EQUAL -> opEqual,
    OP_EQUALVERIFY -> opEqualVerify,
    OP_1ADD -> op1add,
    OP_1SUB -> op1sub,
    OP_NEGATE -> opNegate,
    OP_ABS -> opAbs,
    OP_NOT -> opNot,
    OP_0NOTEQUAL -> op0notEqual,
    OP_ADD -> opAdd,
    OP_SUB -> opSub,
    OP_MUL -> opMul,
    OP_BOOLAND -> opBoolAnd,
    OP_BOOLOR -> opBoolOr,
    OP_NUMEQUAL -> opNumEqual,
    OP_NUMEQUALVERIFY -> opNumEqualVerify,
    OP_NUMNOTEQUAL -> opNumNotEqual,
    OP_LESSTHAN -> opLessThan,
    OP_GREATERTHAN -> opGreaterThan,
    OP_LESSTHANOREQUAL -> opLessThanOrEqual,
    OP_GREATERTHANOREQUAL -> opGreaterThanOrEqual,
    OP_MIN -> opMin,
    OP_MAX -> opMax,
    OP_WITHIN -> opWithin,
    OP_RIPEMD160 -> opRipeMD160,
    OP_SHA1 -> opSha1,
    OP_SHA256 -> opSha256,
    OP_HASH160 -> opHash160,
    OP_HASH256 -> opHash256,
    OP_CHECKSIG -> opCheckSig,
    OP_CHECKSIGVERIFY -> opCheckSigVerify,
    OP_CHECKMULTISIG -> opCheckMultisig,
    OP_CHECKMULTISIGVERIFY -> opCheckMultisigVerify,
    OP_NOP1 -> opNop,
    OP_CHECKLOCKTIMEVERIFY -> opCheckLocktimeVerify,
    OP_CHECKSEQUENCEVERIFY -> opCheckSequenceVerify,
    OP_NOP4 -> opNop,
    OP_NOP5 -> opNop,
    OP_NOP6 -> opNop,
    OP_NOP7 -> opNop,
    OP_NOP8 -> opNop,
    OP_NOP9 -> opNop,
    OP_NOP10 -> opNop
  )

trait PopBranches:
  import Op.*

  private val popToEndif: Op[Seq[Cmd]] =
    popCmd flatMap:
      case OP_ENDIF =>
        pure(Seq())
      case cmd @ (OP_IF | OP_NOTIF) =>
        for
          ifCmds <- popIf(cmd)
          cmds <- popToEndif
        yield (ifCmds ++ cmds)
      case cmd =>
        popToEndif map (cmd +: _)

  private def popIf(ifCmd: Cmd): Op[Seq[Cmd]] =
    popToEndif map:
      ifCmd +: _ :+ OP_ENDIF

  protected val popBranches: Op[(Seq[Cmd], Seq[Cmd])] =
    popCmd flatMap:
      case OP_ELSE =>
        popToEndif map: falseBranch =>
          (Seq(), falseBranch)
      case OP_ENDIF =>
        pure((Seq(), Seq()))
      case cmd @ (OP_IF | OP_NOTIF) =>
        for
          ifCmds <- popIf(cmd)
          (trueBranch, falseBranch) <- popBranches
        yield (ifCmds ++ trueBranch, falseBranch)
      case cmd =>
        popBranches map: (trueBranch, falseBranch) =>
          (cmd +: trueBranch, falseBranch)
