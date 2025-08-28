package AES128

import chisel3._

class AES extends Module {
  val io = IO(new Bundle {
    val blockIn  = Input(Vec(16, UInt(8.W)))
    val blockOut = Output(Vec(16, UInt(8.W)))
    val start    = Input(Bool())
    val done     = Output(Bool())
  })

  val rk = VecInit(Seq(
    VecInit(Seq(0x00,0x01,0x02,0x03, 0x04,0x05,0x06,0x07,
      0x08,0x09,0x0a,0x0b, 0x0c,0x0d,0x0e,0x0f).map(_.U(8.W))),
    VecInit(Seq(0xd6,0xaa,0x74,0xfd, 0xd2,0xaf,0x72,0xfa,
      0xda,0xa6,0x78,0xf1, 0xd6,0xab,0x76,0xfe).map(_.U(8.W))),
    VecInit(Seq(0xb6,0x92,0xcf,0x0b, 0x64,0x3d,0xbd,0xf1,
      0xbe,0x9b,0xc5,0x00, 0x68,0x30,0xb3,0xfe).map(_.U(8.W))),
    VecInit(Seq(0xb6,0xff,0x74,0x4e, 0xd2,0xc2,0xc9,0xbf,
      0x6c,0x59,0x0c,0xbf, 0x04,0x69,0xbf,0x41).map(_.U(8.W))),
    VecInit(Seq(0x47,0xf7,0xf7,0xbc, 0x95,0x35,0x3e,0x03,
      0xf9,0x6c,0x32,0xbc, 0xfd,0x05,0x8d,0xfd).map(_.U(8.W))),
    VecInit(Seq(0x3c,0xaa,0xa3,0xe8, 0xa9,0x9f,0x9d,0xeb,
      0x50,0xf3,0xaf,0x57, 0xad,0xf6,0x22,0xaa).map(_.U(8.W))),
    VecInit(Seq(0x5e,0x39,0x0f,0x7d, 0xf7,0xa6,0x92,0x96,
      0xa7,0x55,0x3d,0xc1, 0x0a,0xa3,0x1f,0x6b).map(_.U(8.W))),
    VecInit(Seq(0x14,0xf9,0x70,0x1a, 0xe3,0x5f,0xe2,0x8c,
      0x44,0x0a,0xdf,0x4d, 0x4e,0xa9,0xc0,0x26).map(_.U(8.W))),
    VecInit(Seq(0x47,0x43,0x87,0x35, 0xa4,0x1c,0x65,0xb9,
      0xe0,0x16,0xba,0xf4, 0xae,0xbf,0x7a,0xd2).map(_.U(8.W))),
    VecInit(Seq(0x54,0x99,0x32,0xd1, 0xf0,0x85,0x57,0x68,
      0x10,0x93,0xed,0x9c, 0xbe,0x2c,0x97,0x4e).map(_.U(8.W))),
    VecInit(Seq(0x13,0x11,0x1d,0x7f, 0xe3,0x94,0x4a,0x17,
      0xf3,0x07,0xa7,0x8b, 0x4d,0x2b,0x30,0xc5).map(_.U(8.W)))
  ))
  val sb  = Module(new SubBytes())
  val sr  = Module(new ShiftRows())
  val mc  = Module(new MixColumns())
  val ark = Module(new AddRoundKey())

  val state  = Reg(Vec(16, UInt(8.W)))
  val round  = RegInit(0.U(4.W))
  val active = RegInit(false.B)
  val doneR  = RegInit(false.B)
  io.done     := doneR
  io.blockOut := state

  sb.io.state_in   := state
  sr.io.state_in   := sb.io.state_out
  mc.io.state_in   := sr.io.state_out
  ark.io.state_in  := mc.io.state_out
  ark.io.roundKey  := rk(round)

  when (io.start && !active) {
    for (i <- 0 until 16) { state(i) := io.blockIn(i) ^ rk(0)(i) }
    round  := 1.U
    active := true.B
    doneR  := false.B
  } .elsewhen (active) {
    when (round < 10.U) {
      state := ark.io.state_out
      round := round + 1.U
    } .otherwise {
      sb.io.state_in  := state
      sr.io.state_in  := sb.io.state_out
      ark.io.state_in := sr.io.state_out
      ark.io.roundKey := rk(10)
      state  := ark.io.state_out
      active := false.B
      doneR  := true.B
    }
  } .otherwise {
    doneR := false.B
  }
}

//Varianta mult mai inceata pentru ca am incercat sa evit folosirea registrelor
/*class AES extends Module {
  val io = IO(new Bundle {
    val blockIn  = Input(Vec(16, UInt(8.W)))
    val blockOut = Output(Vec(16, UInt(8.W)))
  })

  val rk = Keys.expandedKey

  val s0 = Wire(Vec(16, UInt(8.W)))
  for (i <- 0 until 16) { s0(i) := io.blockIn(i) ^ rk(0)(i) }

  val states = Wire(Vec(11, Vec(16, UInt(8.W))))
  states(0) := s0

  val sb = Seq.fill(10)(Module(new SubBytes()))
  val sr = Seq.fill(10)(Module(new ShiftRows()))
  val mc = Seq.fill(9)(Module(new MixColumns()))
  val ak = Seq.fill(10)(Module(new AddRoundKey()))

  for (r <- 1 to 9) {
    sb(r-1).io.state_in := states(r-1)
    sr(r-1).io.state_in := sb(r-1).io.state_out
    mc(r-1).io.state_in := sr(r-1).io.state_out
    ak(r-1).io.state_in := mc(r-1).io.state_out
    ak(r-1).io.roundKey := rk(r)
    states(r) := ak(r-1).io.state_out
  }
  sb(9).io.state_in := states(9)
  sr(9).io.state_in := sb(9).io.state_out
  ak(9).io.state_in := sr(9).io.state_out
  ak(9).io.roundKey := rk(10)
  states(10) := ak(9).io.state_out

  io.blockOut := states(10)
}*/