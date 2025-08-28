package AESUnitTests

import AES128._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AddRoundKeysUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AddRoundKey"
  it should "XOR state with roundKey byte-wise" in {
    test(new AddRoundKey()) { dut =>
      val state = (0 until 16).map(_.toByte & 0xff)
      val key   = (0 until 16).map(i => (0xf0 + i).toByte & 0xff)
      val exp   = state.zip(key).map{ case (s,k) => (s ^ k) & 0xff }

      for (i <- 0 until 16) {
        dut.io.in(i).poke(state(i).U)
        dut.io.rKey(i).poke(key(i).U)
      }

      dut.clock.step()
      for (i <- 0 until 16) {
        dut.io.out(i).expect(exp(i).U)
      }
    }
  }
}