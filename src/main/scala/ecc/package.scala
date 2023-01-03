package ecc

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest
import scala.util.Random

def hash256(message: String): String = String(hash256(message.getBytes()))

def hash256(message: Array[Byte]): Array[Byte] = sha256(sha256(message))

def hmacSHA256(secret: String, message: String): String =
  String(hmacSHA256(secret.getBytes(), message.getBytes()))

def hmacSHA256(secret: Array[Byte], message: Array[Byte]): Array[Byte] =
  val secretKey = SecretKeySpec(secret, "SHA256")
  val mac = Mac.getInstance("SHA256")
  mac.init(secretKey)
  mac.doFinal(message)

def sha256(message: String): String = String(sha256(message.getBytes()))

def sha256(message: Array[Byte]): Array[Byte] =
  MessageDigest.getInstance("SHA-256").digest(message)
