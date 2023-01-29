package tx

import helper.{Parser => P, Serializer => S, _}

final case class Script(cmds: Seq[Cmd] = Seq()):

  def ++ (that: Script) = Script(this.cmds ++ that.cmds)

  val raw_serialize = S.traverse(cmds) {
    case opCode: OpCode =>
      LittleEndian.serialize(opCode.code, 1)

    case elem: Elem =>
      assert(elem.length <= 520, "too long a cmd")

      (
        if elem.length <= 75 then
          LittleEndian.serialize(elem.length, 1)
        else if elem.length < 0x100 then
          LittleEndian.serialize(76, 1) >> LittleEndian.serialize(elem.length, 1)
        else
          LittleEndian.serialize(77, 1) >> LittleEndian.serialize(elem.length, 2)
      ) >> S.tell(elem)
  }

  val serialize =
    val result = raw_serialize()
    VarInt.serialize(result.length) >> S.tell(result)

  def evaluate(z: BigInt): Boolean =
    var ctx = Op.Context(List[Elem](), cmds, z, List[Elem]())
  
    while ctx.cmds.nonEmpty do
      val cmd = ctx.cmds.head
      ctx = ctx.copy(cmds = cmds.tail)

      cmd match
        case opCode: OpCode =>
          Op.get(opCode) flatMap (_ apply ctx) match
            case Some(newCtx) => ctx = newCtx
            case None => return false

        case elem: Elem =>
          ctx = ctx.copy(stack = elem :: ctx.stack)

    ctx.stack match
      case e :: _ => e.nonEmpty
      case _ => false


object Script:

  private def parsePushData(n: Int): P[Cmd] = for {
    dataLength <- LittleEndian.parse(n)
    data <- P.takeBytes(dataLength.toInt)
  } yield data

  private val parseCmd: P[Cmd] = for {
    current <- P.unsigned(1)
    cmd <- current.toInt match {
      case n if n >= 1 && n <= 75 => P.takeBytes(n)
      case 76 => parsePushData(1)
      case 77 => parsePushData(2)
      case opCode => P.pure(OpCode.fromCode(opCode))
    }
  } yield cmd

  val parse: P[Script] = for {
    length <- VarInt.parse
    cmds <- parseCmd.many(length.toInt)
  } yield Script(cmds)
