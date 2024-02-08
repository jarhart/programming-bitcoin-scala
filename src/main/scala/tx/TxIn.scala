package tx

import helper.*

case class TxIn(
    prevTx: Array[Byte],
    prevIndex: BigInt,
    scriptSig: Script,
    sequence: BigInt = BigInt("ffffffff", 16)
):
  val encode =
    for
      _ <- Bytes.tellReverse(prevTx)
      _ <- LittleEndian.encode(prevIndex, 4)
      _ <- scriptSig.encode
      _ <- LittleEndian.encode(sequence, 4)
    yield ()

  def serialize = encode.written.toArray

  def fetchTx(testnet: Boolean = false): Tx =
    TxFetcher.fetch(formatHex(prevTx), testnet)

  def value(testnet: Boolean = false): BigInt =
    fetchTx(testnet).outputs(prevIndex.toInt).amount

  def scriptPubkey(testnet: Boolean = false): Script =
    fetchTx(testnet).outputs(prevIndex.toInt).script

  def replaceScriptSigWithPubkey(testnet: Boolean = false): TxIn =
    copy(scriptSig = scriptPubkey(testnet))

object TxIn:
  val decode: Decoder[TxIn] =
    for
      prevTx <- Bytes.takeReverse(32)
      prevIndex <- LittleEndian.decode(4)
      scriptSig <- Script.decode
      sequence <- LittleEndian.decode(4)
    yield TxIn(prevTx, prevIndex, scriptSig, sequence)

  val parse = Decoder.run(decode)
