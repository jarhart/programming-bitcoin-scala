package ecc

final case class Signature(r: BigInt, s: BigInt):
  import Signature.{sigMarker, valMarker}

  def der: Array[Byte] =
    val rBin = r.toByteArray
    val sBin = s.toByteArray

    val sig =
      Array(valMarker, rBin.length.toByte) ++ rBin
        ++ Array(valMarker, sBin.length.toByte) ++ sBin

    Array(sigMarker, sig.length.toByte) ++ sig

object Signature:
  val sigMarker: Byte = 0x30
  val valMarker: Byte = 0x02

  def parse(bytes: Array[Byte]): Signature =
    require(bytes(0) == sigMarker, "Bad Signature")
    val sig = bytes drop 2
    require(bytes(1) == sig.length, "Bad Signature Length")
    require(sig(0) == valMarker, "Bad Signature")
    val rLength = sig(1)
    val (rBin, sPart) = sig drop 2 splitAt rLength
    require(sPart(0) == valMarker, "Bad Signature")
    val sLength = sPart(1)
    val sBin = sPart drop 2
    require(sLength == sBin.length, "Bad Signature")
    Signature(r = BigInt(rBin), s = BigInt(sBin))
