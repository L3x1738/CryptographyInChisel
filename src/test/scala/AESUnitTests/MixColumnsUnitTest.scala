package AESUnitTests

import AES128._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MixColumnsUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MixColumns"
  it should "transform a known column as in FIPS-197 example" in {
    test(new MixColumns()) { dut =>
      val zero = 0x00
      val in = Array(
        0xdb, 0x13, 0x53, 0x45,
        zero, zero, zero, zero,
        zero, zero, zero, zero,
        zero, zero, zero, zero
      )
      val exp = Array(
        0x8e, 0x4d, 0xa1, 0xbc,
        zero, zero, zero, zero,
        zero, zero, zero, zero,
        zero, zero, zero, zero
      )

      for (i <- 0 until 16) dut.io.in(i).poke(in(i).U)
      dut.clock.step()
      for (i <- 0 until 16) dut.io.out(i).expect(exp(i).U)
    }
  }
}
