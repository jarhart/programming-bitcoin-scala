package tx.script

import ecc._
import helper._
import collection.mutable
import math._

import cats.syntax.flatMap._

object Ops:
  import Op._

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

  val opNop = liftU(Some(_))

  val opIf = liftCU { (stack, cmds) =>
    val items = mutable.Buffer[Cmd](cmds*)
    val trueItems = mutable.Buffer[Cmd]()
    val falseItems = mutable.Buffer[Cmd]()
    var currentItems: mutable.Buffer[Cmd] = trueItems
    var found = false
    var numEndifsNeeded = 1

    while items.nonEmpty && !found do
      val item = items.remove(0)
      if item == OP_IF || item == OP_NOTIF then
        numEndifsNeeded += 1
        currentItems += item
      else if numEndifsNeeded == 1 && item == OP_ELSE then
        currentItems = falseItems
      else if item == OP_ENDIF then
        if numEndifsNeeded == 1 then
          found = true
        else
          numEndifsNeeded -= 1
          currentItems += item
      else
        currentItems += item
    
    Option.when(found)(stack) collect {
      case e :: stack =>
        (stack, (if (Num.decode(e) == 0) then falseItems else trueItems).toSeq)
    }
  }

  val opNotIf = liftCU { (stack, cmds) =>
    val items = mutable.Buffer[Cmd](cmds*)
    val trueItems = mutable.Buffer[Cmd]()
    val falseItems = mutable.Buffer[Cmd]()
    var currentItems: mutable.Buffer[Cmd] = trueItems
    var found = false
    var numEndifsNeeded = 1

    while items.nonEmpty && !found do
      val item = items.remove(0)
      if item == OP_IF || item == OP_NOTIF then
        numEndifsNeeded += 1
        currentItems += item
      else if numEndifsNeeded == 1 && item == OP_ELSE then
        currentItems = falseItems
      else if item == OP_ENDIF then
        if numEndifsNeeded == 1 then
          found = true
        else
          numEndifsNeeded -= 1
          currentItems += item
      else
        currentItems += item
    
    Option.when(found)(stack) collect {
      case e :: stack =>
        (stack, (if (Num.decode(e) == 0) then trueItems else falseItems).toSeq)
    }
  }

  val opVerify = liftU { case element :: stack if Num.decode(element) != 0 => stack }

  val opReturn = liftU(_ => None)

  val opToAltStack = liftSU { case (x :: stack, altStack) => (stack, x :: altStack) }

  val opFromAltStack = liftSU { case (stack, x :: altStack) => (x :: stack, altStack) }

  val op2drop = liftU { case _ :: _ :: stack => stack }

  val op2dup = liftU { case stack @ (a :: b :: _) => (a :: b :: stack) }

  val op3dup = liftU { case stack @ (a :: b :: c :: _) => (a :: b :: c :: stack) }

  val op2over = liftU { case stack @ (_ :: _ :: a :: b :: _) => (a :: b :: stack) }

  val op2rot = liftU { case a :: b :: c :: d :: e :: f :: stack => (e :: f :: a :: b :: c :: d :: stack) }

  val op2swap = liftU { case a :: b :: c :: d :: stack => (c :: d :: a :: b :: stack) }

  val opIfDup = liftU { case e :: stack => (if Num.decode(e) == 0 then stack else e :: stack) }

  val opDepth = liftU { case stack => (Num.encode(stack.length) :: stack) }

  val opDrop = liftU { case _ :: stack => stack }

  val opDup = liftU { case stack @ (e :: _) => (e :: stack) }

  val opNip = liftU { case a :: _ :: stack => (a :: stack) }

  val opOver = liftU { case stack @ (_ :: a :: _) => (a :: stack) }

  val opPick = liftU { stack =>
    for (e <- stack.headOption; n = Num.decode(e).toInt if stack.tail.isDefinedAt(n))
      yield (stack.tail(n) :: stack.tail)
  }

  val opRoll = liftU { stack =>
    for (e <- stack.headOption; n = Num.decode(e).toInt if stack.tail.isDefinedAt(n))
      yield stack.tail.splitAt(n) match
        case (s1, s2) => (s2.head :: s1 ++ s2.tail)
  }

  val opRot = liftU { case a :: b :: c :: stack => (c :: a :: b :: stack) }

  val opSwap = liftU { case a :: b :: stack => (b :: a :: stack) }

  val opTuck = liftU { case a :: b :: stack => (a :: b :: b :: stack) }

  val opSize = liftU { case stack @ (e :: _) => (Num.encode(e.length) :: stack) }

  val opEqual = binary((element1, element2) => Num.encode(if element1 == element2 then 1 else 0))

  val opEqualVerify = opEqual >> opVerify

  val op1add = intUnary(_ + 1)

  val op1sub = intUnary(_ - 1)

  val opNegate = intUnary(- _)

  val opAbs = intUnary(_.abs)

  val opNot = intUnary(i => if i == 0 then 1 else 0)

  val op0notEqual = intUnary(i => if i == 0 then 0 else 1)

  val opAdd = intBinary(_ + _)

  val opSub = intBinary(_ - _)

  val opMul = intBinary(_ * _)

  val opBoolAnd = intBinary ((i, j) => if i != 0 && j != 0 then 1 else 0)
  
  val opBoolOr = intBinary ((i, j) => if i != 0 || j != 0 then 1 else 0)

  val opNumEqual = intBinary((i, j) => if i == j then 1 else 0)

  val opNumEqualVerify = opNumEqual >> opVerify

  val opNumNotEqual = intBinary((i, j) => if i == j then 0 else 1)

  val opLessThan = intBinary((i, j) => if i < j then 1 else 0)

  val opGreaterThan = intBinary((i, j) => if i > j then 1 else 0)

  val opLessThanOrEqual = intBinary((i, j) => if i <= j then 1 else 0)

  val opGreaterThanOrEqual = intBinary((i, j) => if i >= j then 1 else 0)

  val opMin = intBinary(_ min _)

  val opMax = intBinary(_ max _)

  val opWithin = intTrinary { (maximum, minimum, element) =>
    if minimum to maximum contains element then 1 else 0
  }

  val opRipeMD160 = unary(ripemd160)

  val opSha1 = unary(sha1)

  val opSha256 = unary(sha256)

  val opHash160 = unary(hash160)
  
  val opHash256 = unary(hash256)

  val opCheckSig = liftZU { (stack, z) =>
    for {
      pk <- stack.headOption flatMap S256Point.parse
      sig <- stack.tail.headOption map Signature.parse
      verified = S256Point.verify(pk, z, sig)
      result = Num.encode(if verified then 1 else 0)
    } yield result :: stack.drop(2)
  }

  val opCheckSigVerify = opCheckSig >> opVerify

  val opCheckMultisig = Op[Unit](_ => ???)

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
    OP_NOTIF-> opNotIf,
    OP_VERIFY -> opVerify,
    OP_RETURN -> opReturn,
    OP_TOALTSTACK -> opToAltStack,
    OP_FROMALTSTACK -> opFromAltStack,
    OP_2DROP -> op2drop,
    OP_2DUP -> op2dup,
    OP_3DUP -> op3dup,
    OP_2OVER -> op2over,
  )
