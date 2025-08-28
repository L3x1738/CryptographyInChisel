package AES128

import chisel3._
import chisel3.util._

class MixColumns() extends Module {
  val io = IO(new Bundle {
    val state_in = Input(Vec(16, UInt(8.W)))
    val state_out = Output(Vec(16, UInt(8.W)))
  })
//pentru calcul in GF(2^8)
  def xtime(x: UInt): UInt = {
    val sh = (x << 1)(7,0)
    Mux(x(7), sh ^ "h1b".U(8.W), sh)
  }
//tabel de multiplicari cu 2 si 3
  val mul02: Vec[UInt] = VecInit((0 until 256).map(i => xtime(i.U(8.W))))
  val mul03: Vec[UInt] = VecInit((0 until 256).map(i => (xtime(i.U(8.W)) ^ i.U(8.W))(7,0)))

  val tmp_state = Wire(Vec(16, UInt(8.W)))
  tmp_state(0) := mul02(io.state_in(0)) ^ mul03(io.state_in(1)) ^ io.state_in(2) ^ io.state_in(3)
  tmp_state(1) := io.state_in(0) ^ mul02(io.state_in(1)) ^ mul03(io.state_in(2)) ^ io.state_in(3)
  tmp_state(2) := io.state_in(0) ^ io.state_in(1) ^ mul02(io.state_in(2)) ^ mul03(io.state_in(3))
  tmp_state(3) := mul03(io.state_in(0)) ^ io.state_in(1) ^ io.state_in(2) ^ mul02(io.state_in(3))

  tmp_state(4) := mul02(io.state_in(4)) ^ mul03(io.state_in(5)) ^ io.state_in(6) ^ io.state_in(7)
  tmp_state(5) := io.state_in(4) ^ mul02(io.state_in(5)) ^ mul03(io.state_in(6)) ^ io.state_in(7)
  tmp_state(6) := io.state_in(4) ^ io.state_in(5) ^ mul02(io.state_in(6)) ^ mul03(io.state_in(7))
  tmp_state(7) := mul03(io.state_in(4)) ^ io.state_in(5) ^ io.state_in(6) ^ mul02(io.state_in(7))

  tmp_state(8) := mul02(io.state_in(8)) ^ mul03(io.state_in(9)) ^ io.state_in(10) ^ io.state_in(11)
  tmp_state(9) := io.state_in(8) ^ mul02(io.state_in(9)) ^ mul03(io.state_in(10)) ^ io.state_in(11)
  tmp_state(10) := io.state_in(8) ^ io.state_in(9) ^ mul02(io.state_in(10)) ^ mul03(io.state_in(11))
  tmp_state(11) := mul03(io.state_in(8)) ^ io.state_in(9) ^ io.state_in(10) ^ mul02(io.state_in(11))

  tmp_state(12) := mul02(io.state_in(12)) ^ mul03(io.state_in(13)) ^ io.state_in(14) ^ io.state_in(15)
  tmp_state(13) := io.state_in(12) ^ mul02(io.state_in(13)) ^ mul03(io.state_in(14)) ^ io.state_in(15)
  tmp_state(14) := io.state_in(12) ^ io.state_in(13) ^ mul02(io.state_in(14)) ^ mul03(io.state_in(15))
  tmp_state(15) := mul03(io.state_in(12)) ^ io.state_in(13) ^ io.state_in(14) ^ mul02(io.state_in(15))

  io.state_out := tmp_state

object MixColumns {
  def apply(): MixColumns = Module(new MixColumns())
}
}
