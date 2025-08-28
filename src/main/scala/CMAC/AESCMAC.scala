package AES128
import chisel3._
import chisel3.util._

class AESCMAC extends Module {
  val io = IO(new Bundle {
    val start     = Input(Bool())
    val msgBlock  = Input(Vec(16, UInt(8.W)))
    val msgValid  = Input(Bool())
    val msgReady  = Output(Bool())
    val msgLast   = Input(Bool())
    val lastBytes = Input(UInt(5.W)) // 0..16
    val mac       = Output(Vec(16, UInt(8.W)))
    val done      = Output(Bool())
  })

  // ====== Subcheile tale fixe ======
  private def bytes(hex: String): Vec[UInt] = {
    require(hex.length == 32, "aștept exact 16 bytes / 32 hex chars")
    val bs = hex.grouped(2).toArray.map(h => BigInt(h,16).U(8.W))
    VecInit(bs)
  }
  // K1, K2 din mesajul tău:
  val K1 = bytes("8d42766f0f1eb704de9f02c54391b075")
  val K2 = bytes("1a84ecde1e3d6e09bd3e058a8723606d")

  // ====== Instanța AES existentă ======
  val aes = Module(new AES())
  aes.io.start   := false.B
  aes.io.blockIn := VecInit(Seq.fill(16)(0.U(8.W)))

  // ====== Utilitare ======
  def xorV(a: Vec[UInt], b: Vec[UInt]): Vec[UInt] =
    VecInit((0 until 16).map(i => a(i) ^ b(i)))

  /** padding 10* pentru ultimul bloc (len ∈ [0..15]) */
  def padBlock(src: Vec[UInt], len: UInt): Vec[UInt] = {
    val out = Wire(Vec(16, UInt(8.W)))
    for (i <- 0 until 16) out(i) := 0.U
    for (i <- 0 until 16) { when (len > i.U) { out(i) := src(i) } }
    when (len < 16.U) {
      val idx = len // 0..15
      out(idx) := "h80".U
    }
    out
  }

  // ====== Registre ======
  val X     = RegInit(VecInit(Seq.fill(16)(0.U(8.W)))) // Step 5: X := 0
  val macR  = Reg(Vec(16, UInt(8.W)))
  io.mac    := macR

  val curBlk  = Reg(Vec(16, UInt(8.W))) // buffer pt blocul curent (stabil cât rulează AES)
  val doneR   = RegInit(false.B); io.done := doneR
  when (doneR) { doneR := false.B } // puls de 1 ciclu

  // ====== FSM (fix pe pașii 2..7) ======
  val sIdle :: sWaitBlk :: sRunMidStart :: sRunMidWait :: sRunLastStart :: sRunLastWait :: sDone :: Nil = Enum(7)
  val st = RegInit(sIdle)

  // handshake
  io.msgReady := (st === sWaitBlk)

  switch (st) {
    // Step 1 & 5: start => X := 0^128 (K1/K2 sunt „date”)
    is (sIdle) {
      when (io.start) {
        for (i <- 0 until 16) X(i) := 0.U
        st := sWaitBlk // Step 2..3 vor fi „implicate” prin felul în care trimiți ultimul bloc
      }
    }

    // Step 6 (partea pentru i=1..n-1): mâncăm blocuri intermediare
    is (sWaitBlk) {
      when (io.msgValid) {
        when (!io.msgLast) {
          // bloc intermediar: Y := X xor M_i; X := AES(K,Y)
          val inBlk = xorV(X, io.msgBlock)
          curBlk := inBlk
          aes.io.blockIn := inBlk
          aes.io.start   := true.B
          st := sRunMidStart
        } .otherwise {
          // bloc final: Step 3/4 + calcule finale
          val full = (io.lastBytes === 16.U)
          val Mi   = Mux(full, io.msgBlock, padBlock(io.msgBlock, io.lastBytes))
          val Mlast= xorV(Mi, Mux(full, K1, K2))  // Step 4
          val inBlk= xorV(X, Mlast)               // Y := M_last xor X
          curBlk := inBlk
          aes.io.blockIn := inBlk
          aes.io.start   := true.B
          st := sRunLastStart
        }
      }
    }

    is (sRunMidStart) { st := sRunMidWait }
    is (sRunMidWait) {
      when (aes.io.done) {
        for (i <- 0 until 16) X(i) := aes.io.blockOut(i) // X := AES-128(K,Y)
        st := sWaitBlk
      }
    }

    is (sRunLastStart) { st := sRunLastWait }
    is (sRunLastWait) {
      when (aes.io.done) {
        for (i <- 0 until 16) macR(i) := aes.io.blockOut(i) // T := AES-128(K,Y)
        st := sDone
      }
    }

    is (sDone) {
      doneR := true.B
      st := sIdle
    }
  }
}
