package ecc

import org.scalatest.freespec.AnyFreeSpec
import java.util.HexFormat

class S256PointSpec extends AnyFreeSpec:

  val hexFormat = HexFormat.of()
  import hexFormat.parseHex

  "S256Point" - {

    "order of g is n" in {
      assert(S256Point.n * S256Point.g == S256Point.atInfinity)
    }

    "multiplying by g produces correct public points" in {
      val points = Seq(
        (BigInt(7), BigInt("5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc", 16), BigInt("6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da", 16)),
        (BigInt(1485), BigInt("c982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda", 16), BigInt("7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55", 16)),
        (BigInt(2).pow(128), BigInt("8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da", 16), BigInt("662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82", 16)),
        (BigInt(2).pow(240) + BigInt(2).pow(31), BigInt("9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116", 16), BigInt("10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053", 16)),
      )

      for ((secret, x, y) <- points)
        val point = S256Point(x, y)
        assert(secret * S256Point.g == point)
    }

    "verify verifies signatures" - {
      val point = S256Point(
          BigInt("887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c", 16),
          BigInt("61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34", 16))

      "case 1" in {
        val z = BigInt("ec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60", 16)
        val r = BigInt("ac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395", 16)
        val s = BigInt("68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4", 16)
        assert(point.verify(z, Signature(r, s)))
      }

      "case 2" in {
        val z = BigInt("7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d", 16)
        val r = BigInt("eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c", 16)
        val s = BigInt("c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6", 16)
        assert(point.verify(z, Signature(r, s)))
      }
    }

    val uncompressed = Seq(
      (BigInt(999).pow(3), "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9"),
      (BigInt(123), "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b"),
      (BigInt(42424242), "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3"),
    )
    
    val compressed = Seq(
      (BigInt(999).pow(3), "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5"),
      (BigInt(123), "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"),
      (BigInt(42424242), "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"),
    )

    "sec" - {

      "encodes in the uncompressed SEC format" in {
        for ((secret, s) <- uncompressed)
          val point = secret * S256Point.g
          assert(point.sec(compressed = false).toSeq == parseHex(s).toSeq)
      }

      "encodes in the compressed SEC format" in {
        for ((secret, s) <- compressed)
          val point = secret * S256Point.g
          assert(point.sec(compressed = true).toSeq == parseHex(s).toSeq)
      }
    }

    "parse" - {

      "parses uncompressed SEC format" in {
        for ((secret, s) <- uncompressed)
          val point = secret * S256Point.g
          assert(S256Point.parse(parseHex(s)) == Some(point))
      }

      "parses compressed SEC format" in {
        for ((secret, s) <- compressed)
          val point = secret * S256Point.g
          assert(S256Point.parse(parseHex(s)) == Some(point))
      }
    }

    "address encodes addresses" in {
      val addresses = Seq(
        (BigInt(888).pow(3), true, "148dY81A9BmdpMhvYEVznrM45kWN32vSCN", "mieaqB68xDCtbUBYFoUNcmZNwk74xcBfTP"),
        (BigInt(321), false, "1S6g2xBJSED7Qr9CYZib5f4PYVhHZiVfj", "mfx3y63A7TfTtXKkv7Y6QzsPFY6QCBCXiP"),
        (BigInt(4242424242L), false, "1226JSptcStqn4Yq9aAmNXdwdc2ixuH9nb", "mgY3bVusRUL6ZB2Ss999CSrGVbdRwVpM8s"),
      )

      for ((secret, compressed, mainnetAddress, testnetAddress) <- addresses)
        val point = secret * S256Point.g
        assert(point.address(compressed=compressed, testnet=false) == mainnetAddress)
        assert(point.address(compressed=compressed, testnet=true) == testnetAddress)
    }
  }
