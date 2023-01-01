package ecc

import math.BigInt

def pow(base: Int, exp: Int): Int =
  BigInt(base).pow(exp).toInt

def modPow(base: Int, exp: Int, m: Int): Int =
  BigInt(base).modPow(BigInt(exp), BigInt(m)).toInt
