import AES128.InvAES
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InvAESUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AES-128 Decryption"
  it should "decrypt NIST FIPS-197 vector" in {
    test(new InvAES()) { dut =>
      val pt = Array(0x00,0x11,0x22,0x33, 0x44,0x55,0x66,0x77, 0x88,0x99,0xaa,0xbb, 0xcc,0xdd,0xee,0xff)
      val ct = Array(0x69,0xc4,0xe0,0xd8, 0x6a,0x7b,0x04,0x30, 0xd8,0xcd,0xb7,0x80, 0x70,0xb4,0xc5,0x5a)
      for (i <- 0 until 16) dut.io.blockIn(i).poke(ct(i).U)
      dut.io.start.poke(true.B); dut.clock.step(); dut.io.start.poke(false.B)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 40) {
        dut.clock.step(); cycles += 1
      }
      assert(dut.io.done.peek().litToBoolean, s"Nu a terminat în $cycles cicluri")
      for (i <- 0 until 16) dut.io.blockOut(i).expect(pt(i).U)
    }
  }
}