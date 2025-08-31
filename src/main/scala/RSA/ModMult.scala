package RSA
import chisel3._
import chisel3.util._

class ModMult(val WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val a = Input(UInt(WIDTH.W))
    val b = Input(UInt(WIDTH.W))
    val n = Input(UInt(WIDTH.W))
    val y = Output(UInt(WIDTH.W))
    val valid = Output(Bool())
    val ready = Output(Bool())
  })

  val idle :: run :: done :: Nil = Enum(3)
  val state = RegInit(idle)

  val A = Reg(UInt((WIDTH + 1).W))
  val B = Reg(UInt(WIDTH.W))
  val N = Reg(UInt((WIDTH + 1).W))
  val Acc = Reg(UInt((WIDTH + 1).W))
  val i = Reg(UInt((WIDTH + 1).W))

  io.y := Acc(WIDTH - 1, 0)
  io.valid := (state === done)
  io.ready := (state === idle)

  switch(state) {
    is(idle) {
      when(io.start) {
        A := Cat(0.U(1.W), io.a)
        B := io.b
        N := Cat(0.U(1.W), io.n)
        Acc := 0.U((WIDTH + 1).W)
        i := 0.U
        state := run
      }
    }
    is(run) {
      when(i < WIDTH.U) {
        when(B(0)) {
          val sum = Acc +& A
          when(sum >= N) {
            Acc := sum - N
          }.otherwise {
            Acc := sum
          }
        }
        val dbl = Cat(A(WIDTH-1, 0), 0.U(1.W))
        when(dbl >= N) {
          A := dbl - N
        }.otherwise {
          A := dbl
        }
        B := B >> 1
        i := i + 1.U
      }.otherwise {
        state := done
      }
    }
    is(done) {
      state := idle
    }
  }
}