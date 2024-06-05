package tx

import helper.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpec

class TxInSpec extends AnyFunSpec with BeforeAndAfterAll:

  override def beforeAll() = TxFetcher.loadCache("tx.cache")

  describe("value"):
    it("gets the outpoint value by looking up the tx hash"):
      val txHash =
        "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
      val txIn = TxIn(parseHex(txHash), 0)
      assert(txIn.value() == 42505594)

  describe("scriptPubkey"):
    it("gets the ScriptPubKey by looking up the tx hash"):
      val txHash =
        "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
      val txIn = TxIn(parseHex(txHash), 0)
      val expected =
        parseHex("1976a914a802fc56c704ce87c42d7c92eb75e7896bdc41ae88ac")
      assert(txIn.scriptPubkey().serialize.toSeq == expected.toSeq)
