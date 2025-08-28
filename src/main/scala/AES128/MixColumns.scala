package AES128

import chisel3._
import chisel3.util._

class MixColumns() extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(16, UInt(8.W)))
    val out = Output(Vec(16, UInt(8.W)))
  })
//pentru calcul in GF(2^8)
  def xtime(x: UInt): UInt = {
    val sh = (x << 1)(7,0)
    Mux(x(7), sh ^ "h1b".U(8.W), sh)
  }
//tabel de multiplicari cu 2 si 3
  val mul02: Vec[UInt] = VecInit((0 until 256).map(i => xtime(i.U(8.W))))
  val mul03: Vec[UInt] = VecInit((0 until 256).map(i => (xtime(i.U(8.W)) ^ i.U(8.W))(7,0)))

  val temp = Wire(Vec(16, UInt(8.W)))
  temp(0) := mul02(io.in(0)) ^ mul03(io.in(1)) ^ io.in(2) ^ io.in(3)
  temp(1) := io.in(0) ^ mul02(io.in(1)) ^ mul03(io.in(2)) ^ io.in(3)
  temp(2) := io.in(0) ^ io.in(1) ^ mul02(io.in(2)) ^ mul03(io.in(3))
  temp(3) := mul03(io.in(0)) ^ io.in(1) ^ io.in(2) ^ mul02(io.in(3))

  temp(4) := mul02(io.in(4)) ^ mul03(io.in(5)) ^ io.in(6) ^ io.in(7)
  temp(5) := io.in(4) ^ mul02(io.in(5)) ^ mul03(io.in(6)) ^ io.in(7)
  temp(6) := io.in(4) ^ io.in(5) ^ mul02(io.in(6)) ^ mul03(io.in(7))
  temp(7) := mul03(io.in(4)) ^ io.in(5) ^ io.in(6) ^ mul02(io.in(7))

  temp(8) := mul02(io.in(8)) ^ mul03(io.in(9)) ^ io.in(10) ^ io.in(11)
  temp(9) := io.in(8) ^ mul02(io.in(9)) ^ mul03(io.in(10)) ^ io.in(11)
  temp(10) := io.in(8) ^ io.in(9) ^ mul02(io.in(10)) ^ mul03(io.in(11))
  temp(11) := mul03(io.in(8)) ^ io.in(9) ^ io.in(10) ^ mul02(io.in(11))

  temp(12) := mul02(io.in(12)) ^ mul03(io.in(13)) ^ io.in(14) ^ io.in(15)
  temp(13) := io.in(12) ^ mul02(io.in(13)) ^ mul03(io.in(14)) ^ io.in(15)
  temp(14) := io.in(12) ^ io.in(13) ^ mul02(io.in(14)) ^ mul03(io.in(15))
  temp(15) := mul03(io.in(12)) ^ io.in(13) ^ io.in(14) ^ mul02(io.in(15))

  io.out := temp

object MixColumns {
  def apply(): MixColumns = Module(new MixColumns())
}
}
