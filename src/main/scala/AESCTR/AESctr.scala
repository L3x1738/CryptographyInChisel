package AESCTR
import AES128.AES
import chisel3._
import chisel3.util._


class AESctr extends Module {
  val io = IO(new Bundle {
    val loadIV = Input(Bool())
    val iv = Input(Vec(16, UInt(8.W)))
    val start = Input(Bool())
    val blockIn = Input(Vec(16, UInt(8.W)))
    val isLast = Input(Bool())
    val lastBytes = Input(UInt(5.W))
    val outBlock = Output(Vec(16, UInt(8.W)))
    val outValid = Output(Bool())

  })


  val aes = Module(new AES)
  aes.io.start   := false.B
  aes.io.blockIn := 0.U.asTypeOf(Vec(16, UInt(8.W)))

  val ivReg = RegInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val inBuf = Reg(Vec(16, UInt(8.W)))
  val lastReg = RegInit(false.B)
  val lastBReg = RegInit(0.U(5.W))
  val outReg = Reg(Vec(16, UInt(8.W)))

  when(io.loadIV) {
    for (i <- 0 until 16) { ivReg(i) := io.iv(i) }
  }

  val idle :: run :: finish :: Nil = Enum(3)
  val st = RegInit(idle)

  io.outBlock := outReg
  io.outValid := (st === finish)

  switch(st) {
    is(idle) {
      when(io.start) {
        for (i <- 0 until 16) { inBuf(i) := io.blockIn(i) }
        lastReg  := io.isLast
        lastBReg := io.lastBytes
        aes.io.blockIn := ivReg
        aes.io.start   := true.B
        st := run
      }
    }

    is(run) {
      when(aes.io.done) {
        for (i <- 0 until 16) {
          val x = (inBuf(i) ^ aes.io.blockOut(i))(7,0)
          outReg(i) := Mux(lastReg && (i.U >= lastBReg), 0.U, x)
        }
        val ctr = Cat(ivReg(12), ivReg(13), ivReg(14), ivReg(15)) + 1.U
        ivReg(12) := (ctr >> 24)(7,0)
        ivReg(13) := (ctr >> 16)(7,0)
        ivReg(14) := (ctr >> 8)(7,0)
        ivReg(15) := ctr(7,0)
        st := finish
      }
    }

    is(finish) {
      st := idle
    }
  }
}
