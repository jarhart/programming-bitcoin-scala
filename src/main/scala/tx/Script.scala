package tx

import cats.syntax.apply._
import cats.syntax.traverse._
import helper._

final case class Script(cmds: Seq[Cmd] = Seq()):

  def ++ (that: Script) = Script(this.cmds ++ that.cmds)

  val rawEncode = cmds traverse {
    case opCode: OpCode =>
      LittleEndian.encode(opCode.code, 1)

    case elem: Elem =>
      assert(elem.length <= 520, "too long a cmd")

      (
        if elem.length <= 75 then
          LittleEndian.encode(elem.length, 1)
        else if elem.length < 0x100 then
          LittleEndian.encode(76, 1) *> LittleEndian.encode(elem.length, 1)
        else
          LittleEndian.encode(77, 1) *> LittleEndian.encode(elem.length, 2)
      ) *> Encoder.tell(elem)
  }

  val encode = VarInt.encode(rawEncode.written.length) *> rawEncode

  def serialize = encode.written.toArray

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

  def decodePushData(n: Int): Decoder[Cmd] = for {
    dataLength <- LittleEndian.decode(n)
    data <- Bytes take dataLength.toInt
  } yield data

  val decodeCmd: Decoder[Cmd] = for {
    current <- LittleEndian.decode(1)
    cmd <- current.toInt match {
      case n if n >= 1 && n <= 75 => Bytes take n
      case 76 => decodePushData(1)
      case 77 => decodePushData(2)
      case opCode => Decoder.pure(OpCode.fromCode(opCode))
    }
  } yield cmd

  val decode: Decoder[Script] = for {
    length <- VarInt.decode
    cmds <- Decoder.many(length.toInt)(decodeCmd)
  } yield Script(cmds)

  val parse = Decoder.run(decode)
