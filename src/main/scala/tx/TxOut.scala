package tx

import helper.*

final case class TxOut(amount: BigInt, script: Script):

  val encode =
    for
      _ <- LittleEndian.encode(amount, 8)
      _ <- script.encode
    yield ()

  def serialize = encode.written.toArray

object TxOut:
  val decode: Decoder[TxOut] =
    for
      amount <- LittleEndian.decode(8)
      script <- Script.decode
    yield TxOut(amount, script)

  val parse = Decoder.run(decode)
