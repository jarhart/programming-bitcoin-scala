package ecc

import math.BigInt
  
def modPow(base: Int, exp: Int, m: Int): Int =
  BigInt(base).modPow(BigInt(exp), BigInt(m)).toInt
