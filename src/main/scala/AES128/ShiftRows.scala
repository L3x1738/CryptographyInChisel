package AES128

import chisel3._

// [[0,4,8,12],                   [[0,4,8,12],
//  [1,5,9,13],  se tranforma in   [5,9,13,1],
//  [2,6,10,14],                   [10,14,2,6],
//  [3,7,11,15]]                   [15,3,7,11]]
class ShiftRows extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(16, UInt(8.W)))
    val out = Output(Vec(16, UInt(8.W)))
  })

  io.out(0) := io.in(0)
  io.out(1) := io.in(5)
  io.out(2) := io.in(10)
  io.out(3) := io.in(15)

  io.out(4) := io.in(4)
  io.out(5) := io.in(9)
  io.out(6) := io.in(14)
  io.out(7) := io.in(3)

  io.out(8) := io.in(8)
  io.out(9) := io.in(13)
  io.out(10) := io.in(2)
  io.out(11) := io.in(7)

  io.out(12) := io.in(12)
  io.out(13) := io.in(1)
  io.out(14) := io.in(6)
  io.out(15) := io.in(11)
}

object ShiftRows {
  def apply(): ShiftRows = Module(new ShiftRows())
}
