package RSAUnitTests

import RSA.ModMult
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ModMultUnitTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "ModMult (big numbers)"

  private def runOnce(dut: ModMult, a: BigInt, b: BigInt, n: BigInt): Unit = {

    val aMod = a.mod(n)
    val bMod = b.mod(n)
    while (!dut.io.ready.peek().litToBoolean) { dut.clock.step() }
    dut.io.a.poke(aMod.U); dut.io.b.poke(bMod.U); dut.io.n.poke(n.U)
    dut.io.start.poke(true.B); dut.clock.step(); dut.io.start.poke(false.B)

    var cycles = 0
    val maxCycles = 200000
    while (!dut.io.valid.peek().litToBoolean && cycles < maxCycles) { dut.clock.step(); cycles += 1 }
    assert(cycles < maxCycles, s"Timeout for a=$a b=$b n=$n")

    val got = dut.io.y.peek().litValue
    val exp = (aMod * bMod) % n
    assert(got == exp, s"Mismatch: got=$got, exp=$exp")
  }

  it should "multiply modulo correctly on 128-bit operands" in {
    test(new ModMult(WIDTH = 128)) { dut =>
      val n  = BigInt("f2d1c3b5a4978675544332211ffeedd", 16) | 1  // asigurăm imparitate

      val a1 = BigInt("e3d2c1b0a998877766554433221100ff", 16)
      val b1 = BigInt("0123456789abcdeffedcba9876543210", 16)
      runOnce(dut, a1, b1, n)
      val a2 = n - 2
      val b2 = n - 3
      runOnce(dut, a2, b2, n)

      val a3 = BigInt("ffffffff00000000ffffffff00000000", 16)
      val b3 = BigInt("00000000ffffffff00000000ffffffff", 16)
      runOnce(dut, a3, b3, n)
    }
  }

  it should "multiply modulo correctly on 256-bit operands" in {
    test(new ModMult(WIDTH = 256)) { dut =>

      val n  = BigInt("cf1b2a3948576acefedcba9876543210ffeeddccbbaa99887766554433221111", 16) | 1
      val a1 = BigInt("a1b2c3d4e5f60718293a4b5c6d7e8f90ffeeddccbbaa99887766554433221100", 16)
      val b1 = BigInt("00112233445566778899aabbccddeeff102030405060708090a0b0c0d0e0f00", 16)
      runOnce(dut, a1, b1, n)

      val a2 = n - BigInt(5)
      val b2 = n - BigInt(7)
      runOnce(dut, a2, b2, n)


      val a3 = BigInt("ffffffffffffffffffffffffffffffff00000000000000000000000000000001", 16)
      val b3 = BigInt("00000000000000000000000000000001ffffffffffffffffffffffffffffffff", 16)
      runOnce(dut, a3, b3, n)
    }
  }
}
