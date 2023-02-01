package tx.script

import math.floorMod

object Num:

  def encode(num: BigInt): Elem =
    if num == 0 then
      Array[Byte]()
    else
      val result = LazyList
        .unfold(num.abs)(absNum => Option.when(absNum != 0)(((absNum & 0xff).toByte, absNum >> 8)))
        .toArray
    
      if (result.last & 0x80) != 0 then
        if num < 0 then
          result :+ 0x80.toByte
        else
          result :+ (0: Byte)
      else if num < 0 then
        result.init :+ (result.last | 0x80).toByte
      else
        result

  def decode(element: Elem): BigInt =
    if element.isEmpty then
      0
    else
      val bigEndian = element.reverse.map(b => BigInt(floorMod(b, 0x100)))
      val negative = (bigEndian.head & 0x80) != 0
      val r0 = if negative then bigEndian.head & 0x7f else bigEndian.head
      val result = bigEndian.tail.foldLeft(r0)((r, c) => (r << 8) + c)
      if negative then -result else result
