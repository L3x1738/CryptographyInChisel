package AES128

import chisel3._
//modul pentru a adauga cheia pentru fiecare runda folosind XOR
class AddRoundKey extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(16, UInt(8.W)))
    val rKey  = Input(Vec(16, UInt(8.W)))
    val out = Output(Vec(16, UInt(8.W)))
  })
  for (i <- 0 until 16) {
    io.out(i) := io.in(i) ^ io.rKey(i)
  }
}

object AddRoundKey {
  def apply(): AddRoundKey = Module(new AddRoundKey())
}