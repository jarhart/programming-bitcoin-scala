package tx

import java.util.HexFormat
import helper.{Parser => P, Serializer => S, _}

final case class TxIn(prevTx: Array[Byte], prevIndex: BigInt, scriptSig: Script, sequence: BigInt = BigInt("ffffffff", 16)):
  private val hexFormat = HexFormat.of()
  import hexFormat.formatHex

  val serialize = for {
    _ <- S.tell(prevTx.reverse)
    _ <- LittleEndian.serialize(prevIndex, 4)
    _ <- scriptSig.serialize
    _ <- LittleEndian.serialize(sequence, 4)
  } yield ()

  def fetchTx(testnet: Boolean = false): Tx =
    TxFetcher.fetch(formatHex(prevTx), testnet)

  def value(testnet: Boolean = false): BigInt =
    fetchTx(testnet).outputs(prevIndex.toInt).amount

  def script(testnet: Boolean = false): Script =
    fetchTx(testnet).outputs(prevIndex.toInt).script

object TxIn:
  val parse = for {
    prevTx <- P.reverseBytes(32)
    prevIndex <- LittleEndian.parse(4)
    scriptSig <- Script.parse
    sequence <- LittleEndian.parse(4)
  } yield TxIn(prevTx, prevIndex, scriptSig, sequence)
