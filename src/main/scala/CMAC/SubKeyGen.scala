package AES128
import chisel3._
import chisel3.util._
//asta ajuta pentru calcularea celor doua subchei ca nu am gasit in documentatie pentru cheia mea 000102030405060708090a0b0c0d0e0f, generat de chat :(
class SubKeyGen(val L_CONST: BigInt = BigInt("c6a13b37878f5b826f4f8162a1c8d879", 16)) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val L = Output(Vec(16, UInt(8.W)))
    val K1 = Output(Vec(16, UInt(8.W)))
    val K2 = Output(Vec(16, UInt(8.W)))
    val done = Output(Bool())
  })

  private def splitBytes(u: UInt): Vec[UInt] =
    VecInit((0 until 16).map(i => u(8*(15 - i) + 7, 8*(15 - i))))
  private def catBytes(v: Vec[UInt]): UInt = Cat(v)
  private def dbl128(bytes: Vec[UInt]): Vec[UInt] = {
    val u = catBytes(bytes)
    val msb = u(127)
    val shifted = (u << 1)(127, 0)
    val rb = "h00000000000000000000000000000087".U(128.W)
    val res = Mux(msb, shifted ^ rb, shifted)
    splitBytes(res)
  }

  val Lvec = splitBytes((L_CONST.U(128.W)))
  io.L := Lvec

  val K1 = Reg(Vec(16, UInt(8.W)))
  val K2 = Reg(Vec(16, UInt(8.W)))
  io.K1 := K1
  io.K2 := K2

  val sIdle :: sOut :: Nil = Enum(2)
  val st = RegInit(sIdle)
  val doneR = RegInit(false.B)
  io.done := doneR
  when (st =/= sOut) { doneR := false.B }

  switch (st) {
    is (sIdle) {
      when (io.start) {
        val k1w = dbl128(Lvec)
        val k2w = dbl128(k1w)
        K1 := k1w
        K2 := k2w
        st := sOut
      }
    }
    is (sOut) {
      doneR := true.B
      st := sIdle
    }
  }
}
