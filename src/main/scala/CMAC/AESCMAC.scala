package AES128
import chisel3._
import chisel3.util._

class AESCMAC extends Module {
  val io = IO(new Bundle {
    val go = Input(Bool())
    val blockIn = Input(Vec(16, UInt(8.W)))
    val isLast = Input(Bool())
    val lastBytes = Input(UInt(5.W))
    val blkDone = Output(Bool())
    val mac = Output(Vec(16, UInt(8.W)))
    val macValid = Output(Bool())
  })

  private def bytes(hex: String): Vec[UInt] =
    VecInit(hex.grouped(2).toArray.map(h => BigInt(h,16).U(8.W)))
  val K1 = bytes("8d42766f0f1eb704de9f02c54391b075")
  val K2 = bytes("1a84ecde1e3d6e09bd3e058a8723606d")

  val aes = Module(new AES())
  aes.io.start := false.B
  aes.io.blockIn := VecInit(Seq.fill(16)(0.U(8.W)))

  private def xor(a: Vec[UInt], b: Vec[UInt]) =
    VecInit((0 until 16).map(i => a(i) ^ b(i)))

  private def padding(src: Vec[UInt], len: UInt) = {
    val out = Wire(Vec(16, UInt(8.W)))
    for (i <- 0 until 16) out(i) := 0.U
    for (i <- 0 until 16) { when (len > i.U) { out(i) := src(i) } }
    when (len < 16.U) { out(len) := "h80".U }
    out
  }

  val X = RegInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val macR = Reg(Vec(16, UInt(8.W)))
  io.mac := macR
  val isLastR = RegInit(false.B)
  val blkDoneR = RegInit(false.B)
  io.blkDone := blkDoneR
  when (blkDoneR) { blkDoneR := false.B }
  val macValR = RegInit(false.B)
  io.macValid := macValR
  when (macValR) { macValR := false.B }

  val idle :: run :: finish :: Nil = Enum(3)
  val st = RegInit(idle)

  switch (st) {
    is (idle) {
      when (io.go) {
        isLastR := io.isLast
        val inBlk = Wire(Vec(16, UInt(8.W)))
        when (!io.isLast) {
          inBlk := xor(X, io.blockIn)
        } .otherwise {
          val full = (io.lastBytes === 16.U)
          val Mi = Mux(full, io.blockIn, padding(io.blockIn, io.lastBytes))
          val Mlast = xor(Mi, Mux(full, K1, K2))
          inBlk := xor(X, Mlast)
        }
        aes.io.blockIn := inBlk
        aes.io.start   := true.B
        st := run
      }
    }

    is (run) {
      when (aes.io.done) {
        when (isLastR) {
          for (i <- 0 until 16) macR(i) := aes.io.blockOut(i)
          st := finish
        } .otherwise {
          for (i <- 0 until 16) X(i) := aes.io.blockOut(i)
          blkDoneR := true.B
          st := idle
        }
      }
    }

    is (finish) {
      macValR := true.B
      st := idle
    }
  }
}
