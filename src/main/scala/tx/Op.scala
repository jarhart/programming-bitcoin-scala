package tx

import ecc._
import helper._
import collection.mutable
import math._

type Op = Op.Context => Option[Op.Context]

type Stack = List[Elem]

type Cmd = OpCode | Elem

type Elem = Array[Byte]

object Op:
  import OpCode._

  case class Context(stack: Stack, cmds: Seq[Cmd], z: BigInt, altStack: Stack)

  def get(opCode: OpCode): Option[Op] = operations get opCode

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

  val opNop: Op = Some(_)

  val opIf = liftC { (stack, cmds) =>
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
        (stack, (if (decodeNum(e) == 0) then falseItems else trueItems).toSeq)
    }
  }

  val opNotIf = liftC { (stack, cmds) =>
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
        (stack, (if (decodeNum(e) == 0) then trueItems else falseItems).toSeq)
    }
  }

  val opVerify = lift { case element :: stack if decodeNum(element) != 0 => stack }

  val opReturn: Op = (_ => None)

  val opToAltStack = liftS { case (x :: stack, altStack) => (stack, x :: altStack) }

  val opFromAltStack = liftS { case (stack, x :: altStack) => (x :: stack, altStack) }

  val op2drop = lift { case _ :: _ :: stack => stack }

  val op2dup = lift { case stack @ (a :: b :: _) => a :: b :: stack }

  val op3dup = lift { case stack @ (a :: b :: c :: _) => a :: b :: c :: stack }

  val op2over = lift { case stack @ (_ :: _ :: a :: b :: _) => a :: b :: stack }

  val op2rot = lift { case a :: b :: c :: d :: e :: f :: stack => e :: f :: a :: b :: c :: d :: stack }

  val op2swap = lift { case a :: b :: c :: d :: stack => c :: d :: a :: b :: stack }

  val opIfDup = lift { case e :: stack => if decodeNum(e) == 0 then stack else e :: stack }

  val opDepth = lift { case stack => encodeNum(stack.length) :: stack }

  val opDrop = lift { case _ :: stack => stack }

  val opDup = lift { case stack @ (e :: _) => e :: stack }

  val opNip = lift { case a :: _ :: stack => a :: stack }

  val opOver = lift { case stack @ (_ :: a :: _) => a :: stack }

  val opPick = lift { stack =>
    for (e <- stack.headOption; n = decodeNum(e).toInt if stack.tail.isDefinedAt(n))
      yield stack.tail(n) :: stack.tail
  }

  val opRoll = lift { stack =>
    for (e <- stack.headOption; n = decodeNum(e).toInt if stack.tail.isDefinedAt(n))
      yield stack.tail.splitAt(n) match
        case (s1, s2) => s2.head :: s1 ++ s2.tail
  }

  val opRot = lift { case a :: b :: c :: stack => c :: a :: b :: stack }

  val opSwap = lift { case a :: b :: stack => b :: a :: stack }

  val opTuck = lift { case a :: b :: stack => a :: b :: b :: stack }

  val opSize = lift { case stack @ (e :: _) => encodeNum(e.length) :: stack }

  val opEqual = binary((element1, element2) => encodeNum(if element1 == element2 then 1 else 0))

  val opEqualVerify: Op = opEqual(_) flatMap opVerify

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

  val opNumEqualVerify: Op = opNumEqual(_) flatMap opVerify

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

  val opCheckSig = liftZ { (stack, z) =>
    for {
      pk <- stack.headOption flatMap S256Point.parse
      sig <- stack.tail.headOption map Signature.parse
      verified = S256Point.verify(pk, z, sig)
      result = encodeNum(if verified then 1 else 0)
    } yield result :: stack.drop(2)
  }

  val opCheckSigVerify: Op = opCheckSig(_) flatMap opVerify

  val opCheckMultisig: Op = _ => ???

  val opCheckMultisigVerify: Op = opCheckMultisig(_) flatMap opVerify

  val opCheckLocktimeVerify: Op = _ => ???

  val opCheckSequenceVerify: Op = _ => ???

  def encodeNum(num: BigInt): Elem =
    if num == 0 then
      Array[Byte]()
    else
      val result = LazyList
        .unfold(num.abs)(absNum => Option.when(absNum != 0)(((absNum & 0xff).toByte, absNum >> 8)))
        .toArray
    
      if (result.last & 0x80) != 0 then
        if num < 0 then
          result :+ 0x80.toByte
        else
          result :+ (0: Byte)
      else if num < 0 then
        result.init :+ (result.last | 0x80).toByte
      else
        result

  def decodeNum(element: Elem): BigInt =
    if element.isEmpty then
      0
    else
      val bigEndian = element.reverse.map(b => BigInt(floorMod(b, 0x100)))
      val negative = (bigEndian.head & 0x80) != 0
      val r0 = if negative then bigEndian.head & 0x7f else bigEndian.head
      val result = bigEndian.tail.foldLeft(r0)((r, c) => (r << 8) + c)
      if negative then -result else result

  def push(element: Elem) = lift(stack => Some(element :: stack))

  def push(num: BigInt): Op = push(encodeNum(num))

  def unary(f: Elem => Elem) = lift { case a :: stack => f(a) :: stack }

  def intUnary(f: BigInt => BigInt) = unary(a => encodeNum(f(decodeNum(a))))

  def binary(f: (Elem, Elem) => Elem) = lift {
    case a :: b :: stack => f(a, b) :: stack
  }

  def intBinary(f: (BigInt, BigInt) => BigInt) =
    binary((x, y) => encodeNum(f(decodeNum(x), decodeNum(y))))

  def trinary(f: (Elem, Elem, Elem) => Elem) = lift {
    case a :: b :: c :: stack => f(a, b, c) :: stack
  }

  def intTrinary(f: (BigInt, BigInt, BigInt) => BigInt) =
    trinary((x, y, z) => encodeNum(f(decodeNum(x), decodeNum(y), decodeNum(z))))

  def lift(f: Stack => Option[Stack]): Op =
    ctx => f(ctx.stack) map (stack => ctx.copy(stack = stack))

  def lift(pf: PartialFunction[Stack, Stack]): Op = lift(pf.lift)

  def liftC(f: (Stack, Seq[Cmd]) => Option[(Stack, Seq[Cmd])]): Op =
    ctx => f(ctx.stack, ctx.cmds) map { case (stack, cmds) => ctx.copy(stack = stack, cmds = cmds) }

  def liftC(pf: PartialFunction[(Stack, Seq[Cmd]), (Stack, Seq[Cmd])]): Op = liftC { (stack, cmds) =>
    Option.when(pf.isDefinedAt((stack, cmds)))(pf((stack, cmds)))
  }

  def liftZ(f: (Stack, BigInt) => Option[Stack]): Op =
    ctx => f(ctx.stack, ctx.z) map (stack => ctx.copy(stack = stack))

  def liftZ(pf: PartialFunction[(Stack, BigInt), Stack]): Op = liftZ { (stack, z) =>
    Option.when(pf.isDefinedAt((stack, z)))(pf(stack, z))
  }

  def liftS(f: (Stack, Stack) => Option[(Stack, Stack)]): Op =
    ctx => f(ctx.stack, ctx.altStack) map {
      case (stack, altStack) => ctx.copy(stack = stack, altStack = altStack)
    }

  def liftS(pf: PartialFunction[(Stack, Stack), (Stack, Stack)]): Op = liftS { (stack, altStack) =>
    Option.when(pf.isDefinedAt((stack, altStack)))(pf((stack, altStack)))
  }

  val operations = Map[OpCode, Op](
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
    OP_NOP10 -> opNop,
  )
