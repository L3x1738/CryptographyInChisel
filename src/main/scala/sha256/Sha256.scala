package sha256
import chisel3._
import chisel3.util._

class Sha256 extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val msg = Input(Vec(64, UInt(8.W)))
    val length = Input(UInt(64.W))
    val hash = Output(Vec(8, UInt(32.W)))
    val valid = Output(Bool())
    val ready = Output(Bool())
  })

  private val K = VecInit(Seq(
    "h428a2f98".U(32.W), "h71374491".U(32.W), "hb5c0fbcf".U(32.W), "he9b5dba5".U(32.W),
    "h3956c25b".U(32.W), "h59f111f1".U(32.W), "h923f82a4".U(32.W), "hab1c5ed5".U(32.W),
    "hd807aa98".U(32.W), "h12835b01".U(32.W), "h243185be".U(32.W), "h550c7dc3".U(32.W),
    "h72be5d74".U(32.W), "h80deb1fe".U(32.W), "h9bdc06a7".U(32.W), "hc19bf174".U(32.W),
    "he49b69c1".U(32.W), "hefbe4786".U(32.W), "h0fc19dc6".U(32.W), "h240ca1cc".U(32.W),
    "h2de92c6f".U(32.W), "h4a7484aa".U(32.W), "h5cb0a9dc".U(32.W), "h76f988da".U(32.W),
    "h983e5152".U(32.W), "ha831c66d".U(32.W), "hb00327c8".U(32.W), "hbf597fc7".U(32.W),
    "hc6e00bf3".U(32.W), "hd5a79147".U(32.W), "h06ca6351".U(32.W), "h14292967".U(32.W),
    "h27b70a85".U(32.W), "h2e1b2138".U(32.W), "h4d2c6dfc".U(32.W), "h53380d13".U(32.W),
    "h650a7354".U(32.W), "h766a0abb".U(32.W), "h81c2c92e".U(32.W), "h92722c85".U(32.W),
    "ha2bfe8a1".U(32.W), "ha81a664b".U(32.W), "hc24b8b70".U(32.W), "hc76c51a3".U(32.W),
    "hd192e819".U(32.W), "hd6990624".U(32.W), "hf40e3585".U(32.W), "h106aa070".U(32.W),
    "h19a4c116".U(32.W), "h1e376c08".U(32.W), "h2748774c".U(32.W), "h34b0bcb5".U(32.W),
    "h391c0cb3".U(32.W), "h4ed8aa4a".U(32.W), "h5b9cca4f".U(32.W), "h682e6ff3".U(32.W),
    "h748f82ee".U(32.W), "h78a5636f".U(32.W), "h84c87814".U(32.W), "h8cc70208".U(32.W),
    "h90befffa".U(32.W), "ha4506ceb".U(32.W), "hbef9a3f7".U(32.W), "hc67178f2".U(32.W)
  ))
  private val H0 = VecInit(Seq(
    "h6a09e667".U(32.W), "hbb67ae85".U(32.W), "h3c6ef372".U(32.W), "ha54ff53a".U(32.W),
    "h510e527f".U(32.W), "h9b05688c".U(32.W), "h1f83d9ab".U(32.W), "h5be0cd19".U(32.W)
  ))

  @inline private def rotr(x: UInt, n: Int): UInt = Cat(x(n-1,0), x(31,n))
  @inline private def add32(a: UInt, b: UInt): UInt = (a +& b)(31,0)
  @inline private def Sigma0(x: UInt): UInt = rotr(x,2) ^ rotr(x,13) ^ rotr(x,22)
  @inline private def Sigma1(x: UInt): UInt = rotr(x,6) ^ rotr(x,11) ^ rotr(x,25)
  @inline private def sigma0(x: UInt): UInt = rotr(x,7) ^ rotr(x,18) ^ ((x >> 3).asUInt.pad(32))
  @inline private def sigma1(x: UInt): UInt = rotr(x,17) ^ rotr(x,19) ^ ((x >> 10).asUInt.pad(32))
  @inline private def ch(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ ((~x).asUInt & z)
  @inline private def maj(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ (x & z) ^ (y & z)

  private val idle :: expandW :: hash :: done :: Nil = Enum(4)
  private val state = RegInit(idle)

  private val W = Reg(Vec(64, UInt(32.W)))
  private val H = Reg(Vec(8, UInt(32.W)))
  private val a = Reg(UInt(32.W));
  private val b = Reg(UInt(32.W))
  private val c = Reg(UInt(32.W));
  private val d = Reg(UInt(32.W))
  private val e = Reg(UInt(32.W));
  private val f = Reg(UInt(32.W))
  private val g = Reg(UInt(32.W));
  private val h = Reg(UInt(32.W))
  private val t = Reg(UInt(7.W))

  private val validReg = RegInit(false.B)
  io.valid := validReg; when(validReg){ validReg := false.B }
  io.ready := (state === idle)
  io.hash  := H

  //fsm forta
  switch (state) {
    is (idle) {
      when (io.start) {
        for (i <- 0 until 8) {
          H(i) := H0(i)
        }
        // initializam hash-ul cu 0 si adaugam 1 dupa 0 si apoi la final adaugam lungimea
        val Lbytes = (io.length >> 3).asUInt
        val bytes  = Wire(Vec(64, UInt(8.W)))
        for (i <- 0 until 64) { bytes(i) := 0.U }
        for (i <- 0 until 64) { when (i.U < Lbytes) { bytes(i) := io.msg(i) } }
        when (Lbytes < 64.U) { bytes(Lbytes) := "h80".U(8.W) }
        bytes(63) := (io.length)(7,0)
        bytes(62) := (io.length >> 8)(7,0)

       /* asta este pentru cand mesajul ar fi mai lung de 512 biti
        for (i <- 0 until 8) {
          val shift = (i * 8).U
          bytes(63 - i) := (Lbits >> shift)(7,0)
        }
        */
        // formam W[0...15]
        for (i <- 0 until 16) {
          val b0 = bytes(4*i)
          val b1 = bytes(4*i + 1)
          val b2 = bytes(4*i + 2)
          val b3 = bytes(4*i + 3)
          W(i) := Cat(b0,b1,b2,b3)
        }

        t := 16.U
        state := expandW
      }
    }

    is (expandW) {
      when (t < 64.U) {
        val w_t2  = W(t - 2.U)
        val w_t7  = W(t - 7.U)
        val w_t15 = W(t - 15.U)
        val w_t16 = W(t - 16.U)
        W(t) := add32(add32(sigma1(w_t2), w_t7), add32(sigma0(w_t15), w_t16))
        t := t + 1.U
      } .otherwise {
        a := H(0); b := H(1); c := H(2); d := H(3)
        e := H(4); f := H(5); g := H(6); h := H(7)
        t := 0.U
        state := hash
      }
    }

    is (hash) {
      when (t < 64.U) {
        val T1 = add32(add32(add32(h, Sigma1(e)), ch(e,f,g)), add32(K(t), W(t)))
        val T2 = add32(Sigma0(a), maj(a,b,c))
        h := g; g := f; f := e; e := add32(d, T1)
        d := c; c := b; b := a; a := add32(T1, T2)
        t := t + 1.U
      } .otherwise {
        H(0) := add32(H(0), a); H(1) := add32(H(1), b)
        H(2) := add32(H(2), c); H(3) := add32(H(3), d)
        H(4) := add32(H(4), e); H(5) := add32(H(5), f)
        H(6) := add32(H(6), g); H(7) := add32(H(7), h)
        state := done
      }
    }

    is (done) {
      validReg := true.B
      state := idle
    }
  }
}
