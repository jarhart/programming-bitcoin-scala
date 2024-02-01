package tx

import helper.*

import cats.syntax.traverse.*

final case class Tx(
    version: BigInt,
    inputs: Seq[TxIn],
    outputs: Seq[TxOut],
    locktime: BigInt,
    testnet: Boolean = false
):
  import HexFormat.formatHex

  val SIGHASH_ALL = BigInt(1)

  def id = formatHex(hash)

  def hash = hash256(serialize).reverse

  val encode =
    for
      _ <- LittleEndian.encode(version, 4)
      _ <- VarInt.encode(inputs.length)
      _ <- inputs traverse (_.encode)
      _ <- VarInt.encode(outputs.length)
      _ <- outputs traverse (_.encode)
      _ <- LittleEndian.encode(locktime, 4)
    yield ()

  def serialize = encode.written.toArray

  def fee: BigInt = inputs.map(_.value(testnet)).sum - outputs.map(_.amount).sum

  def verify: Boolean = (fee >= 0) && (inputs.indices forall verifyInput)

  def sigHash(inputIndex: Int): BigInt =
    LittleEndian.toInt:
      hash256:
        replaceScriptSigWithPubkey(inputIndex).serialize
          ++ toBytes(SIGHASH_ALL, 4)

  def verifyInput(inputIndex: Int): Boolean =
    val input = inputs(inputIndex)
    (input.scriptSig ++ input.scriptPubkey(testnet))
      .evaluate(sigHash(inputIndex))

  def replaceScriptSigWithPubkey(inputIndex: Int): Tx =
    copy(inputs =
      inputs.updated(
        inputIndex,
        inputs(inputIndex).replaceScriptSigWithPubkey(testnet)
      )
    )

object Tx:
  val decode: Decoder[Tx] =
    for
      version <- LittleEndian.decode(4)
      numInputs <- VarInt.decode map (_.toInt)
      inputs <- Decoder.times(numInputs)(TxIn.decode)
      numOutputs <- VarInt.decode map (_.toInt)
      outputs <- Decoder.times(numOutputs)(TxOut.decode)
      locktime <- LittleEndian.decode(4)
    yield Tx(version, inputs, outputs, locktime)

  val parse = Decoder.run(decode)
