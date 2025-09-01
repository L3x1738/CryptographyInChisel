package AES128Tests

import AESCTR.AESctr
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer

class AESCTRUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AESctr"

  it should "match expected ciphertext" in {
    test(new AESctr) { dut =>
      val pt = "Buna ziua doamnelor si domnilor acesta este modulul meu AESCTR care sa fie destul de lung".getBytes("ASCII").map(_ & 0xff)
      val iv = Array(0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f)
      val expectedHex = "48e165d46114993090e3f037a73e843f6e0c9eb4157152f2f5b7936627cb7dfc7b4ef1c0657d859dceb6ad6da1468832217ec7f2f43aea272366772b13e7f36dec419408355aa889ba9d85416071770c6798631781a55f5b43"
      val expected = expectedHex.grouped(2).map(Integer.parseInt(_,16)).toArray
      assert(expected.length == pt.length)

      dut.io.loadIV.poke(true.B)
      for (i <- 0 until 16) { dut.io.iv(i).poke(iv(i).U(8.W)) }
      dut.clock.step()
      dut.io.loadIV.poke(false.B)

      val out = ArrayBuffer[Int]()
      var i = 0
      while (i < pt.length) {
        val blkLen = math.min(16, pt.length - i)
        var j = 0
        while (j < 16) {
          val v = if (j < blkLen) pt(i + j) else 0
          dut.io.blockIn(j).poke(v.U(8.W))
          j += 1
        }
        dut.io.isLast.poke(((i + blkLen) == pt.length).B)
        dut.io.lastBytes.poke(blkLen.U)
        dut.io.start.poke(true.B); dut.clock.step(); dut.io.start.poke(false.B)
        while (!dut.io.outValid.peek().litToBoolean) { dut.clock.step() }
        var k = 0
        while (k < blkLen) {
          out += (dut.io.outBlock(k).peek().litValue.toInt & 0xff)
          k += 1
        }
        dut.clock.step()
        i += blkLen
      }

      val ciphertext = out.toArray
      assert(ciphertext.sameElements(expected))
      println(ciphertext.map(b => f"$b%02x").mkString)
    }
  }
}
