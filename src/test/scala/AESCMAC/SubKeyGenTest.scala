import AES128._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SubKeyGenTest extends AnyFlatSpec with ChiselScalatestTester {
  "SubKeyGenConst" should "print L, K1, K2" in {
    test(new SubKeyGen()) { dut =>
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      dut.io.start.poke(true.B); dut.clock.step(1); dut.io.start.poke(false.B)
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      val Lhex  = (0 until 16).map(i => f"${dut.io.L(i).peek().litValue.toInt}%02x").mkString
      val K1hex = (0 until 16).map(i => f"${dut.io.K1(i).peek().litValue.toInt}%02x").mkString
      val K2hex = (0 until 16).map(i => f"${dut.io.K2(i).peek().litValue.toInt}%02x").mkString
      println(s"L  = $Lhex")
      println(s"K1 = $K1hex")
      println(s"K2 = $K2hex")
    }
  }
}
