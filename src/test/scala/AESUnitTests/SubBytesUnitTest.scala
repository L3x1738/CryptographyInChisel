package AESUnitTests

import AES128._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SubBytesUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SubBytes"
  it should "map selected bytes according to AES S-Box" in {
    test(new SubBytes()) { dut =>
      val in  = Array(0x00, 0x01, 0x53, 0xff) ++ Array.fill(12)(0x00)
      val exp = Array(0x63, 0x7c, 0xed, 0x16) ++ Array.fill(12)(0x63)

      for (i <- 0 until 16) dut.io.state_in(i).poke(in(i).U)
      dut.clock.step()
      for (i <- 0 until 16) dut.io.state_out(i).expect(exp(i).U)
    }
  }
}
