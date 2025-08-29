import AES128.AESCMAC
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AESCMACUnitTest extends AnyFlatSpec with ChiselScalatestTester {

  private def readHex16(get: Int => BigInt): String =
    (0 until 16).map(i => f"${get(i).toInt}%02x").mkString

  it should "compute CMAC for one full block (00..0f)" in {
    test(new AESCMAC()) { dut =>

      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val blk = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.blockIn(i).poke(blk(i))
      dut.io.isLast.poke(true.B)
      dut.io.lastBytes.poke(16.U)
      dut.io.go.poke(true.B); dut.clock.step(1); dut.io.go.poke(false.B)
      while (!dut.io.macValid.peek().litToBoolean) { dut.clock.step(1) }
      val macHex = readHex16(i => dut.io.mac(i).peek().litValue)
      println(s"CMAC(000102030405060708090a0b0c0d0e0f) = $macHex")
    }
  }

  it should "compute CMAC for 2 full blocks (00..0f | 10..1f)" in {
    test(new AESCMAC()) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val b1 = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.blockIn(i).poke(b1(i))
      dut.io.isLast.poke(false.B)
      dut.io.lastBytes.poke(16.U)
      dut.io.go.poke(true.B); dut.clock.step(1); dut.io.go.poke(false.B)
      while (!dut.io.blkDone.peek().litToBoolean) { dut.clock.step(1) }
      val b2 = (16 until 32).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.blockIn(i).poke(b2(i))
      dut.io.isLast.poke(true.B)
      dut.io.lastBytes.poke(16.U)
      dut.io.go.poke(true.B); dut.clock.step(1); dut.io.go.poke(false.B)
      while (!dut.io.macValid.peek().litToBoolean) { dut.clock.step(1) }
      val macHex = readHex16(i => dut.io.mac(i).peek().litValue)
      println(s"CMAC(000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f) = $macHex")
    }
  }

  it should "compute CMAC for 2 blocks, last incomplete (5 bytes)" in {
    test(new AESCMAC()) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val b1 = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.blockIn(i).poke(b1(i))
      dut.io.isLast.poke(false.B)
      dut.io.lastBytes.poke(16.U)
      dut.io.go.poke(true.B); dut.clock.step(1); dut.io.go.poke(false.B)
      while (!dut.io.blkDone.peek().litToBoolean) { dut.clock.step(1) }
      val b2 = Seq(16,17,18,19,20) ++ Seq.fill(11)(0)
      for (i <- 0 until 16) dut.io.blockIn(i).poke(b2(i).U(8.W))
      dut.io.isLast.poke(true.B)
      dut.io.lastBytes.poke(5.U)
      dut.io.go.poke(true.B); dut.clock.step(1); dut.io.go.poke(false.B)
      while (!dut.io.macValid.peek().litToBoolean) { dut.clock.step(1) }
      val macHex = readHex16(i => dut.io.mac(i).peek().litValue)
      println(s"CMAC(000102030405060708090a0b0c0d0e0f1011121314) = $macHex")
    }
  }
}
