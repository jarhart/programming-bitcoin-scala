package tx

import helper.{Parser => P, Serializer => S}
import java.util.HexFormat

import helper.hash256
final case class Tx(version: BigInt, inputs: Seq[TxIn], outputs: Seq[TxOut], locktime: BigInt, testnet: Boolean = false):
  private val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  def id = formatHex(hash)

  def hash = hash256(serialize().toArray).reverse

  val serialize = for {
    _ <- S.littleEndian(version, 4)
    _ <- S.varInt(inputs.length)
    _ <- S.traverse(inputs)(_.serialize)
    _ <- S.varInt(outputs.length)
    _ <- S.traverse(outputs)(_.serialize)
    _ <- S.littleEndian(locktime, 4)
  } yield ()

  def fee: BigInt =
    inputs.map(_.value(testnet)).sum - outputs.map(_.amount).sum

object Tx:
  val parse = for {
    version <- P.littleEndian(4)
    numInputs <- P.varInt
    inputs <- P.times(numInputs)(TxIn.parse)
    numOutputs <- P.varInt
    outputs <- P.times(numOutputs)(TxOut.parse)
    locktime <- P.littleEndian(4)
  } yield Tx(version, inputs, outputs, locktime)
