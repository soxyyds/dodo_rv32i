package DODO.BPU

import chisel3._
import chisel3.util.Cat
import Const._

// global history register
class GHR extends Module {
  val io = IO(new Bundle {
    val branch  = Input(Bool())
    val taken   = Input(Bool())
    val ghr     = Output(UInt(GHR_WIDTH.W))
  })

  val ghr = Reg(UInt(GHR_WIDTH.W))

  when (io.branch) {
    ghr := Cat(ghr(GHR_WIDTH - 2, 0), io.taken)
  }

  io.ghr := ghr
}
