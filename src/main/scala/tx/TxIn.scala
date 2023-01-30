package tx

import helper._

import java.util.HexFormat

final case class TxIn(prevTx: Array[Byte], prevIndex: BigInt, scriptSig: Script, sequence: BigInt = BigInt("ffffffff", 16)):
  private val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  val encode = for {
    _ <- Bytes.tellReverse(prevTx)
    _ <- LittleEndian.encode(prevIndex, 4)
    _ <- scriptSig.encode
    _ <- LittleEndian.encode(sequence, 4)
  } yield ()

  def serialize = encode.written.toArray

  def fetchTx(testnet: Boolean = false): Tx =
    TxFetcher.fetch(formatHex(prevTx), testnet)

  def value(testnet: Boolean = false): BigInt =
    fetchTx(testnet).outputs(prevIndex.toInt).amount

  def script(testnet: Boolean = false): Script =
    fetchTx(testnet).outputs(prevIndex.toInt).script

object TxIn:
  val decode: Decoder[TxIn] = for {
    prevTx <- Bytes.takeReverse(32)
    prevIndex <- LittleEndian.decode(4)
    scriptSig <- Script.decode
    sequence <- LittleEndian.decode(4)
  } yield TxIn(prevTx, prevIndex, scriptSig, sequence)

  val parse = Decoder.run(decode)
