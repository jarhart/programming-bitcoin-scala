package tx

import ecc.*
import helper.*

import cats.syntax.traverse.*

final case class Tx(
    version: BigInt,
    inputs: Seq[TxIn],
    outputs: Seq[TxOut],
    locktime: BigInt,
    testnet: Boolean = false
):
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

  def signInput(inputIndex: Int, privateKey: PrivateKey): (Tx, Boolean) =
    val z = sigHash(inputIndex)
    val der = privateKey.sign(z).der
    val sig = der ++ toBytes(SIGHASH_ALL, 1)
    val sec = privateKey.point.sec()
    val updatedTxIn = inputs(inputIndex).copy(scriptSig = Script(sig, sec))
    val signed = copy(inputs = inputs.updated(inputIndex, updatedTxIn))
    (signed, signed.verifyInput(inputIndex))

  def encodeSigHash(inputIndex: Int) =
    for
      _ <- LittleEndian.encode(version, 4)
      _ <- VarInt.encode(inputs.length)
      _ <- inputs.zipWithIndex traverse: (txIn, i) =>
        txIn
          .copy(scriptSig =
            if i == inputIndex then txIn.scriptPubkey(testnet)
            else Script()
          )
          .encode
      _ <- VarInt.encode(outputs.length)
      _ <- outputs traverse (_.encode)
      _ <- LittleEndian.encode(locktime, 4)
      _ <- LittleEndian.encode(SIGHASH_ALL, 4)
    yield ()

  def sigHash(inputIndex: Int): BigInt =
    unsignedFromBytes(
      hash256(encodeSigHash(inputIndex).written.toArray)
    )

  def verifyInput(inputIndex: Int): Boolean =
    val txIn = inputs(inputIndex)
    val scriptPubkey = txIn.scriptPubkey(testnet)
    (txIn.scriptSig ++ scriptPubkey).evaluate(sigHash(inputIndex))

object Tx:
  def decode(testnet: Boolean = false): Decoder[Tx] =
    for
      version <- LittleEndian.decode(4)
      numInputs <- VarInt.decode map (_.toInt)
      inputs <- Decoder.times(numInputs)(TxIn.decode)
      numOutputs <- VarInt.decode map (_.toInt)
      outputs <- Decoder.times(numOutputs)(TxOut.decode)
      locktime <- LittleEndian.decode(4)
    yield Tx(version, inputs, outputs, locktime, testnet)

  def parse(input: IterableOnce[Byte], testnet: Boolean = false) =
    Decoder.run(decode(testnet))(input)
