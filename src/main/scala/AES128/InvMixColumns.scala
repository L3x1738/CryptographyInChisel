package AES128


import chisel3._


/*
 * [0e 0b 0d 09]   [a]
 * [09 0e 0b 0d] * [b]
 * [0d 09 0e 0b]   [c]
 * [0b 0d 09 0e]   [d]
  conform cu specificațiile AES FIPS-197
* */

class InvMixColumns extends Module {
  val io = IO(new Bundle {
    val state_in  = Input(Vec(16, UInt(8.W)))
    val state_out = Output(Vec(16, UInt(8.W)))
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

  val out = Wire(Vec(16, UInt(8.W)))
  for (base <- 0 until 16 by 4) {
    val a = io.state_in(base + 0)
    val b = io.state_in(base + 1)
    val c = io.state_in(base + 2)
    val d = io.state_in(base + 3)

    out(base + 0) := m14(a) ^ m11(b) ^ m13(c) ^ m9(d)
    out(base + 1) := m9(a)  ^ m14(b) ^ m11(c) ^ m13(d)
    out(base + 2) := m13(a) ^ m9(b)  ^ m14(c) ^ m11(d)
    out(base + 3) := m11(a) ^ m13(b) ^ m9(c)  ^ m14(d)
  }

  io.state_out := out
}

object InvMixColumns {
  def apply(): InvMixColumns = Module(new InvMixColumns())
}