package sha256

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Sha256UnitTest extends AnyFlatSpec with ChiselScalatestTester {
  //Functii ajutatoare pentru conversii
  def stringToBytes(input: String): Array[Int] = {
    input.getBytes("UTF-8").map(_.toInt & 0xFF)
  }

  def hashToHexString(hashWords: Array[BigInt]): String = {
    hashWords.map(word => f"${word.toLong}%08x").mkString("")
  }

  def hexStringToWords(hexString: String): Array[BigInt] = {
    require(hexString.length == 64, "SHA-256 hash trebuie să aibă 64 caractere hex")
    (0 until 8).map { i =>
      val start = i * 8
      val end = start + 8
      BigInt(hexString.substring(start, end), 16)
    }.toArray
  }

  def testSha256(input: String, expectedHashHex: String): Unit = {
    test(new Sha256) { dut =>
      val inputBytes = stringToBytes(input)
      val expectedWords = hexStringToWords(expectedHashHex)

      for (i <- 0 until 64) dut.io.msg(i).poke(0.U)
      for (i <- inputBytes.indices) {
        dut.io.msg(i).poke(inputBytes(i).U)
      }
      dut.io.length.poke((inputBytes.length * 8).U)
      while (!dut.io.ready.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      var cycles = 0
      while (!dut.io.valid.peek().litToBoolean && cycles < 300) {
        dut.clock.step()
        cycles += 1
      }

      for (i <- 0 until 8) {
        dut.io.hash(i).expect(expectedWords(i).U(32.W))
      }
    }
  }

  behavior of "SHA-256 (single-block, start/valid)"

  it should "match FIPS 180-4 vector for \"abc\"" in {
    testSha256(
      input = "abc",
      expectedHashHex = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
  }



  /*
  test manual pentru ca nu stiu sa fac functii...
  it should "manual test for \"abc\" (original method)" in {
    test(new Sha256) { dut =>
      // PT = "abc"
      val pt = Array(0x61, 0x62, 0x63)
      val expWords = Array(
        0xBA7816BFL, 0x8F01CFEAL, 0x414140DEL, 0x5DAE2223L,
        0xB00361A3L, 0x96177A9CL, 0xB410FF61L, 0xF20015ADL
      ).map(BigInt(_))

      for (i <- 0 until 64) dut.io.msgBytes(i).poke(0.U)
      for (i <- pt.indices)  dut.io.msgBytes(i).poke(pt(i).U)
      dut.io.messageLen.poke((pt.length * 8).U)

      while (!dut.io.ready.peek().litToBoolean) { dut.clock.step() }
      dut.io.start.poke(true.B); dut.clock.step(); dut.io.start.poke(false.B)

      var cycles = 0
      while (!dut.io.valid.peek().litToBoolean && cycles < 300) {
        dut.clock.step(); cycles += 1
      }

      for (i <- 0 until 8) {
        dut.io.hash(i).expect(expWords(i).U(32.W))
      }
    }
  }
*/
}
