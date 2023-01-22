package tx

import scala.collection.mutable
import scala.io.Source

import play.api.libs.json._

import helper.LittleEndian

import java.util.HexFormat
import java.io.{File, FileInputStream, PrintWriter}

object TxFetcher:

  private val cache = mutable.Map[String, Tx]()

  val hexFormat = HexFormat.of()
  import hexFormat.{formatHex, parseHex}

  val mainnetUrl = "https://blockstream.info/api"
  val testnetUrl = "https://blockstream.info/testnet/api"
  
  def getUrl(testnet: Boolean = false) =
    if testnet then testnetUrl else mainnetUrl

  def fetch(txId: String, testnet: Boolean = false, fresh: Boolean = false): Tx =
    fetchFromCache(txId, fresh) getOrElse storeInCache(txId, fetchFromSource(txId, fresh))

  def loadCache(filename: String): Unit =
    val diskCache: Map[String, String] = Json.fromJson(Json.parse(FileInputStream(filename))).get
    cache.clear()
    cache.addAll(diskCache map { case k -> rawHex => k -> parseTx(LazyList from parseHex(rawHex)) })

  def dumpCache(filename: String): Unit =
    val diskCache = cache map { case k -> tx => k -> formatHex(tx.serialize().toArray) }
    val writer = PrintWriter(File(filename))
    writer.write(Json.stringify(Json.toJson(diskCache)))
    writer.close()

  private def fetchFromCache(txId: String, fresh: Boolean): Option[Tx] =
    if fresh then None else cache.get(txId)

  private def storeInCache(txId: String, tx: Tx): Tx =
    cache.put(txId, tx)
    tx

  private def fetchFromSource(txId: String, testnet: Boolean): Tx =
    val tx = parseTx(fetchBytes(txId, testnet))
    assert(tx.id == txId, s"not the same id: ${tx.id} vs ${txId}")
    tx

  private def parseTx(bytes: LazyList[Byte]): Tx =
    if bytes(4) == 0 then
      Tx.parse(bytes.take(4) ++ bytes.drop(6))
        .copy(locktime = LittleEndian.toInt(bytes.takeRight(4)))
    else
      Tx.parse(bytes)

  private def fetchBytes(txId: String, testnet: Boolean): LazyList[Byte] =
    LazyList.from(
      Source.fromURL(s"${getUrl(testnet)}/tx/${txId}/hex")
        .grouped(2)
        .map(_.toArray)
        .flatMap(parseHex(_, 0, 2)))
