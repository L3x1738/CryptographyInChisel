package AESUnitTests

import AES128._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ShiftRowsUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ShiftRows"
  it should "rotate rows by (row index) positions to the left" in {
    test(new ShiftRows()) { dut =>
      for (i <- 0 until 16) dut.io.state_in(i).poke(i.U)
      val expectedIdx = Array(0,5,10,15, 4,9,14,3, 8,13,2,7, 12,1,6,11)
      dut.clock.step()
      for (i <- 0 until 16) {
        dut.io.state_out(i).expect(expectedIdx(i).U)
      }
    }
  }
}