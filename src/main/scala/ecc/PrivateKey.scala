package ecc

import helper.*
import scala.util.Random

final case class PrivateKey(secret: BigInt):
  import S256Point.{g, n}

  require(secret > 1 && secret < n)

  import PrivateKey.randomK

  val point = secret * g

  def hex =
    val s = secret.toString(16)
    new String(Array.fill(64 - s.length())('0')) ++ s

  def sign(z: BigInt, rnd: Random = Random): Signature =
    val (k, r) = randomKeyPair(rnd)
    val kInv = k.modPow(n - 2, n)
    val s = (z + r * secret) * kInv.mod(n)
    Signature(r, if s > n / 2 then n - s else s)

  def wif(compressed: Boolean = true, testnet: Boolean = false) =
    val secretBytes = toBytes(secret)
    val prefix = if testnet then 0xef.toByte else 0x80.toByte
    val suffix: Array[Byte] = if compressed then Array(1) else Array()
    Base58check.encode(prefix +: (secretBytes ++ suffix))

  private def deterministicKeyPair(z: BigInt): (BigInt, BigInt) =
    val k = deterministicK(z)
    k * g match
      case NonZeroPoint(FieldElement(r), _) => k -> r
      case _                                => ???

  private def deterministicK(z: BigInt): BigInt =
    if z > n then deterministicK(z - n)
    else
      val k0 = Array.fill[Byte](32)(0)
      val v0 = Array.fill[Byte](32)(1)
      val zBytes = toBytes(z)
      val secretBytes = toBytes(secret)
      val k1 = hmacSHA256(k0, (v0 :+ (0: Byte)) ++ secretBytes ++ zBytes)
      val v1 = hmacSHA256(k1, v0)
      val k2 = hmacSHA256(k1, (v1 :+ (1: Byte)) ++ secretBytes ++ zBytes)
      val v2 = hmacSHA256(k2, v1)
      detKTail(k2, v2)

  private def detKTail(k: Array[Byte], v: Array[Byte]): BigInt =
    val v1 = hmacSHA256(k, v)
    BigInt(v1) match
      case candidate if candidate > 1 && candidate < n =>
        candidate
      case _ =>
        val k2 = hmacSHA256(k, v :+ (0: Byte))
        val v2 = hmacSHA256(k2, v)
        detKTail(k2, v2)

  private def randomKeyPair(rnd: Random = Random): (BigInt, BigInt) =
    val k = randomK(rnd)
    k * g match
      case NonZeroPoint(FieldElement(r), _) => k -> r
      case _                                => randomKeyPair(rnd)

object PrivateKey:
  import S256Point.n

  def random(rnd: Random = Random) = PrivateKey(randomK(rnd))

  private[PrivateKey] def randomK(rnd: Random): BigInt =
    BigInt(256, rnd) match
      case i if i < n => i
      case _          => randomK(rnd)
