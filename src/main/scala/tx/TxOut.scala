package tx

import helper.{Parser => P, Serializer => S}

final case class TxOut(amount: BigInt, script: Script):
  def serialize = for {
    _ <- S.littleEndian(amount, 8)
    _ <- script.serialize
  } yield ()

object TxOut:
  val parse: P[TxOut] = for {
    amount <- P.littleEndian(8)
    script <- Script.parse
  } yield TxOut(amount, script)
