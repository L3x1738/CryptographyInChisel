package AESCTR
import AES128.AES
import chisel3._
import chisel3.util._


class AESctr extends Module {
  val io = IO(new Bundle {
    val loadIV = Input(Bool())
    val iv = Input(Vec(16, UInt(8.W)))
    val go = Input(Bool())
    val blockIn = Input(Vec(16, UInt(8.W)))
    val isLast = Input(Bool())
    val lastBytes = Input(UInt(5.W))          // 0..16
    val outBlock = Output(Vec(16, UInt(8.W)))
    val outValid = Output(Bool())

  })


  val aes = Module(new AES)
  aes.io.start   := false.B
  aes.io.blockIn := 0.U.asTypeOf(Vec(16, UInt(8.W))) // default

  // Registre strict necesare
  val ivReg = RegInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val inBuf = Reg(Vec(16, UInt(8.W)))
  val lastReg = RegInit(false.B)
  val lastBReg = RegInit(0.U(5.W))
  val outReg = Reg(Vec(16, UInt(8.W)))

  // IV load
  when(io.loadIV) {
    for (i <- 0 until 16) { ivReg(i) := io.iv(i) }
  }

  // FSM minimal
  val sIdle :: sRun :: sFinish :: Nil = Enum(3)
  val st = RegInit(sIdle)

  // ieșirile derivă din stări
  io.outBlock := outReg
  io.outValid := (st === sFinish)

  switch(st) {
    is(sIdle) {
      when(io.go) {
        // capturează intrarea pentru blocul curent
        for (i <- 0 until 16) { inBuf(i) := io.blockIn(i) }
        lastReg  := io.isLast
        lastBReg := io.lastBytes
        // pornește AES pe IV curent (keystream)
        aes.io.blockIn := ivReg
        aes.io.start   := true.B
        st := sRun
      }
    }

    is(sRun) {
      when(aes.io.done) {
        // XOR keystream cu datele; pentru ultimul bloc ia doar lastBytes
        for (i <- 0 until 16) {
          val x = (inBuf(i) ^ aes.io.blockOut(i))(7,0)
          outReg(i) := Mux(lastReg && (i.U >= lastBReg), 0.U, x)
        }
        // post-increment counter (ultimii 4 bytes big-endian)
        val ctr = Cat(ivReg(12), ivReg(13), ivReg(14), ivReg(15)) + 1.U
        ivReg(12) := (ctr >> 24)(7,0)
        ivReg(13) := (ctr >> 16)(7,0)
        ivReg(14) := (ctr >> 8)(7,0)
        ivReg(15) := ctr(7,0)
        // emite 1 ciclu
        st := sFinish
      }
    }

    is(sFinish) {
      // outValid/blkDone sunt high în acest ciclu prin logica de mai sus
      st := sIdle
    }
  }
}
