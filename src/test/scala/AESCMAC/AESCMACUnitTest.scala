import AES128.{AES, AESCMAC}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AESCMACUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  "CMAC_KnownSubkeys" should "compute and print CMAC for one block" in {
    test(new AESCMAC) { dut =>
      // reset
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)

      // 1) start sesiunea
      dut.io.start.poke(true.B); dut.clock.step(1); dut.io.start.poke(false.B)

      // 2) așteaptă să fie gata să primească bloc
      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // 3) pregătim un bloc de exemplu (0x00..0x0f)
      val blk = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk(i))

      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(true.B)    // e și ultimul bloc
      dut.io.lastBytes.poke(16.U)    // bloc plin
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // 4) așteptăm rezultatul
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      val macBytes = (0 until 16).map(i => dut.io.mac(i).peek().litValue.toInt)
      val macHex   = macBytes.map(b => f"$b%02x").mkString
      println(s"CMAC(00..0f) = $macHex")
    }
  }

  it should "compute CMAC for 2 full blocks" in {
    test(new AESCMAC) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)

      // start
      dut.io.start.poke(true.B); dut.clock.step(1); dut.io.start.poke(false.B)

      // așteaptă ready
      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // Blocul 1: 00..0f
      val blk1 = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk1(i))
      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(false.B)  // NU e ultimul
      dut.io.lastBytes.poke(16.U)
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // așteaptă iar ready pentru blocul 2
      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // Blocul 2: 10..1f
      val blk2 = (16 until 32).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk2(i))
      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(true.B)   // acesta e ultimul
      dut.io.lastBytes.poke(16.U)   // bloc plin
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // așteaptă done
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      val macHex = (0 until 16).map(i => f"${dut.io.mac(i).peek().litValue.toInt}%02x").mkString
      println(s"CMAC(2 blocuri) = $macHex")
    }
  }

  it should "compute CMAC for 1 incomplete block (5 bytes)" in {
    test(new AESCMAC) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)

      // start
      dut.io.start.poke(true.B); dut.clock.step(1); dut.io.start.poke(false.B)

      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // Blocul final conține doar 5 bytes: 01 02 03 04 05
      val blk = Seq(1,2,3,4,5) ++ Seq.fill(11)(0)
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk(i).U(8.W))

      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(true.B)   // ultimul bloc
      dut.io.lastBytes.poke(5.U)    // doar 5 octeți sunt valizi
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // așteptăm rezultatul
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      val macHex = (0 until 16).map(i => f"${dut.io.mac(i).peek().litValue.toInt}%02x").mkString
      println(s"CMAC(5 bytes) = $macHex")
    }
  }
  it should "compute CMAC for 2 blocks, second incomplete (5 bytes)" in {
    test(new AESCMAC) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)

      // start sesiunea
      dut.io.start.poke(true.B); dut.clock.step(1); dut.io.start.poke(false.B)

      // așteaptă ready
      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // Primul bloc: 16 bytes 00..0f
      val blk1 = (0 until 16).map(_.U(8.W))
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk1(i))
      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(false.B) // nu e ultimul
      dut.io.lastBytes.poke(16.U)
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // așteaptă iar ready
      while (!dut.io.msgReady.peek().litToBoolean) { dut.clock.step(1) }

      // Al doilea bloc: doar 5 bytes valizi (10..14)
      val blk2 = Seq(16,17,18,19,20) ++ Seq.fill(11)(0)
      for (i <- 0 until 16) dut.io.msgBlock(i).poke(blk2(i).U(8.W))
      dut.io.msgValid.poke(true.B)
      dut.io.msgLast.poke(true.B)   // acesta e ultimul
      dut.io.lastBytes.poke(5.U)    // doar 5 bytes valizi
      dut.clock.step(1)
      dut.io.msgValid.poke(false.B)

      // așteptăm done
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      val macHex = (0 until 16).map(i => f"${dut.io.mac(i).peek().litValue.toInt}%02x").mkString
      println(s"CMAC(2 blocuri, al 2-lea incomplet 5B) = $macHex")
    }
  }


}
