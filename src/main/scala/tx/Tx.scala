package tx

import helper.{Parser => P, Serializer => S, _}
import java.util.HexFormat

import helper.hash256
final case class Tx(version: BigInt, inputs: Seq[TxIn], outputs: Seq[TxOut], locktime: BigInt, testnet: Boolean = false):
  private val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  def id = formatHex(hash)

  def hash = hash256(serialize().toArray).reverse

  val serialize = for {
    _ <- LittleEndian.serialize(version, 4)
    _ <- VarInt.serialize(inputs.length)
    _ <- S.traverse(inputs)(_.serialize)
    _ <- VarInt.serialize(outputs.length)
    _ <- S.traverse(outputs)(_.serialize)
    _ <- LittleEndian.serialize(locktime, 4)
  } yield ()

  def fee: BigInt =
    inputs.map(_.value(testnet)).sum - outputs.map(_.amount).sum

object Tx:
  val parse = for {
    version <- LittleEndian.parse(4)
    numInputs <- VarInt.parse
    inputs <- P.times(numInputs)(TxIn.parse)
    numOutputs <- VarInt.parse
    outputs <- P.times(numOutputs)(TxOut.parse)
    locktime <- LittleEndian.parse(4)
  } yield Tx(version, inputs, outputs, locktime)
