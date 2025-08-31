package AES128
import chisel3._
import chisel3.util._

/*
 * Inverse MixColumns pentru AES (FIPS-197):
 *   [0e 0b 0d 09]   [a]
 *   [09 0e 0b 0d] * [b]
 *   [0d 09 0e 0b]   [c]
 *   [0b 0d 09 0e]   [d]
 */
class InvMixColumns extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(16, UInt(8.W)))
    val out = Output(Vec(16, UInt(8.W)))
  })

  private def xtime(x: UInt): UInt = {
    val sh = (x << 1)(7,0)
    Mux(x(7), sh ^ "h1b".U(8.W), sh)
  }

  private def m2(x: UInt)  = xtime(x)
  private def m4(x: UInt)  = xtime(m2(x))
  private def m8(x: UInt)  = xtime(m4(x))
  private def m9(x: UInt)  = (m8(x) ^ x)(7,0)
  private def m11(x: UInt) = (m8(x) ^ m2(x) ^ x)(7,0)
  private def m13(x: UInt) = (m8(x) ^ m4(x) ^ x)(7,0)
  private def m14(x: UInt) = (m8(x) ^ m4(x) ^ m2(x))(7,0)

  private val mul09: Vec[UInt] = VecInit((0 until 256).map(i => m9 (i.U(8.W))))
  private val mul0b: Vec[UInt] = VecInit((0 until 256).map(i => m11(i.U(8.W))))
  private val mul0d: Vec[UInt] = VecInit((0 until 256).map(i => m13(i.U(8.W))))
  private val mul0e: Vec[UInt] = VecInit((0 until 256).map(i => m14(i.U(8.W))))

  val temp = Wire(Vec(16, UInt(8.W)))

  temp(0) := mul0e(io.in(0)) ^ mul0b(io.in(1)) ^ mul0d(io.in(2)) ^ mul09(io.in(3))
  temp(1) := mul09(io.in(0)) ^ mul0e(io.in(1)) ^ mul0b(io.in(2)) ^ mul0d(io.in(3))
  temp(2) := mul0d(io.in(0)) ^ mul09(io.in(1)) ^ mul0e(io.in(2)) ^ mul0b(io.in(3))
  temp(3) := mul0b(io.in(0)) ^ mul0d(io.in(1)) ^ mul09(io.in(2)) ^ mul0e(io.in(3))

  temp(4) := mul0e(io.in(4)) ^ mul0b(io.in(5)) ^ mul0d(io.in(6)) ^ mul09(io.in(7))
  temp(5) := mul09(io.in(4)) ^ mul0e(io.in(5)) ^ mul0b(io.in(6)) ^ mul0d(io.in(7))
  temp(6) := mul0d(io.in(4)) ^ mul09(io.in(5)) ^ mul0e(io.in(6)) ^ mul0b(io.in(7))
  temp(7) := mul0b(io.in(4)) ^ mul0d(io.in(5)) ^ mul09(io.in(6)) ^ mul0e(io.in(7))

  temp(8)  := mul0e(io.in(8))  ^ mul0b(io.in(9))  ^ mul0d(io.in(10)) ^ mul09(io.in(11))
  temp(9)  := mul09(io.in(8))  ^ mul0e(io.in(9))  ^ mul0b(io.in(10)) ^ mul0d(io.in(11))
  temp(10) := mul0d(io.in(8))  ^ mul09(io.in(9))  ^ mul0e(io.in(10)) ^ mul0b(io.in(11))
  temp(11) := mul0b(io.in(8))  ^ mul0d(io.in(9))  ^ mul09(io.in(10)) ^ mul0e(io.in(11))

  temp(12) := mul0e(io.in(12)) ^ mul0b(io.in(13)) ^ mul0d(io.in(14)) ^ mul09(io.in(15))
  temp(13) := mul09(io.in(12)) ^ mul0e(io.in(13)) ^ mul0b(io.in(14)) ^ mul0d(io.in(15))
  temp(14) := mul0d(io.in(12)) ^ mul09(io.in(13)) ^ mul0e(io.in(14)) ^ mul0b(io.in(15))
  temp(15) := mul0b(io.in(12)) ^ mul0d(io.in(13)) ^ mul09(io.in(14)) ^ mul0e(io.in(15))

  io.out := temp
}

object InvMixColumns {
  def apply(): InvMixColumns = Module(new InvMixColumns())
}
