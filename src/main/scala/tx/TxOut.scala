package tx

import helper.{Parser => P, Serializer => S, _}

final case class TxOut(amount: BigInt, script: Script):
  def serialize = for {
    _ <- LittleEndian.serialize(amount, 8)
    _ <- script.serialize
  } yield ()

object TxOut:
  val parse: P[TxOut] = for {
    amount <- LittleEndian.parse(8)
    script <- Script.parse
  } yield TxOut(amount, script)
