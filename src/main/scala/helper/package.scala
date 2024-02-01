package helper

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest

import scala.util.Random
import java.security.Security
import org.bouncycastle.jce.provider.BouncyCastleProvider

def hash160(message: String): String = String(hash160(message.getBytes()))

def hash160(message: Array[Byte]): Array[Byte] = ripemd160(sha256(message))

def hash256(message: String): String = String(hash256(message.getBytes()))

def hash256(message: Array[Byte]): Array[Byte] = sha256(sha256(message))

def hmacSHA256(secret: String, message: String): String =
  String(hmacSHA256(secret.getBytes(), message.getBytes()))

def hmacSHA256(secret: Array[Byte], message: Array[Byte]): Array[Byte] =
  val secretKey = SecretKeySpec(secret, "SHA256")
  val mac = Mac.getInstance("SHA256")
  mac.init(secretKey)
  mac.doFinal(message)

def sha1(message: String): String = String(sha1(message.getBytes()))

def sha1(message: Array[Byte]): Array[Byte] =
  MessageDigest.getInstance("SHA-1").digest(message)

def sha256(message: String): String = String(sha256(message.getBytes()))

def sha256(message: Array[Byte]): Array[Byte] =
  MessageDigest.getInstance("SHA256").digest(message)

def ripemd160(message: String): String = String(ripemd160(message.getBytes()))

def ripemd160(message: Array[Byte]): Array[Byte] =
  Security.addProvider(BouncyCastleProvider())
  MessageDigest.getInstance("RIPEMD160").digest(message)

def toBytes(x: BigInt, length: Int = 32): Array[Byte] =
  val unPadded = x.toByteArray match
    case Array(0, rest*) => rest.toArray
    case other           => other
  require(unPadded.length <= length, s"${x} is too big for ${length} bytes")
  padLeft(length)(unPadded)

def unsignedFromBytes(bytes: Array[Byte]) = BigInt((0: Byte) +: bytes)

def unsigned(b: Byte): Int = math.floorMod(b, 0x100)

def padLeft(length: Int)(bytes: Array[Byte]): Array[Byte] =
  Array.fill(length - bytes.length)(0: Byte) ++ bytes
