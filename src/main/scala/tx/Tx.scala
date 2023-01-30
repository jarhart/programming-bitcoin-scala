package tx

import helper._

import cats.syntax.traverse._

import java.util.HexFormat

final case class Tx(version: BigInt, inputs: Seq[TxIn], outputs: Seq[TxOut], locktime: BigInt, testnet: Boolean = false):
  private val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  def id = formatHex(hash)

  def hash = hash256(serialize).reverse

  val encode = for {
    _ <- LittleEndian.encode(version, 4)
    _ <- VarInt.encode(inputs.length)
    _ <- inputs traverse (_.encode)
    _ <- VarInt.encode(outputs.length)
    _ <- outputs traverse (_.encode)
    _ <- LittleEndian.encode(locktime, 4)
  } yield ()

  def serialize = encode.written.toArray

  def fee: BigInt =
    inputs.map(_.value(testnet)).sum - outputs.map(_.amount).sum

object Tx:
  val decode: Decoder[Tx] = for {
    version <- LittleEndian.decode(4)
    numInputs <- VarInt.decode map (_.toInt)
    inputs <- Decoder.times(numInputs)(TxIn.decode)
    numOutputs <- VarInt.decode map (_.toInt)
    outputs <- Decoder.times(numOutputs)(TxOut.decode)
    locktime <- LittleEndian.decode(4)
  } yield Tx(version, inputs, outputs, locktime)

  val parse = Decoder.run(decode)
