package DODO.BPU

import chisel3._
import Const._
import chisel3.util.log2Ceil


  // Branch Information
class BranchIO extends Bundle {
  val branch  = Bool()              // branch
  val jump    = Bool()              // jal or jalr
  val taken   = Bool()              // taken or not
  val index   = UInt(GHR_WIDTH.W)   // last index of PHT
  val pc      = UInt(ADDR_WIDTH.W)  // last instruction PC
  val target  = UInt(ADDR_WIDTH.W)  // last branch target
}

class BP extends Module {
  val io = IO(new Bundle {
    val branchIO  = Input(new BranchIO)
    val lookupPc    = Input(UInt(ADDR_WIDTH.W))
    val predTaken   = Output(Bool())
    val predTarget  = Output(UInt(ADDR_WIDTH.W))
    val predIndex   = Output(UInt(GHR_WIDTH.W))
  })


  // Connect
  val ghr = Module(new GHR)
  val pht = Module(new PHT)
  val btb = Module(new BTB)

  ghr.io.branch := io.branchIO.branch
  ghr.io.taken  := io.branchIO.taken


  val index = io.lookupPc(GHR_WIDTH + ADDR_ALIGN_WIDTH - 1,
    ADDR_ALIGN_WIDTH) ^ ghr.io.ghr    // G-share
  pht.io.lastBranch := io.branchIO.branch
  pht.io.lastTaken  := io.branchIO.taken
  pht.io.lastIndex  := io.branchIO.index
  pht.io.index      := index


  btb.io.branch   := io.branchIO.branch
  btb.io.jump     := io.branchIO.jump
  btb.io.pc       := io.branchIO.pc
  btb.io.target   := io.branchIO.target
  btb.io.lookupPc := io.lookupPc


  // Output
  io.predTaken  := btb.io.lookupBranch &&
    (pht.io.taken || btb.io.lookupJump)
  io.predTarget := btb.io.lookupTarget
  io.predIndex  := index
}


  // Constant Definition
object Const {
  val ADDR_WIDTH        = 64
  val ADDR_ALIGN_WIDTH  = log2Ceil(ADDR_WIDTH / 8)
  val GHR_WIDTH         = 5
  val PHT_SIZE          = 1 << GHR_WIDTH
  val BTB_INDEX_WIDTH   = 7
  val BTB_PC_WIDTH      = ADDR_WIDTH - BTB_INDEX_WIDTH - ADDR_ALIGN_WIDTH
  val BTB_TARGET_WIDTH  = ADDR_WIDTH - ADDR_ALIGN_WIDTH
  val BTB_SIZE          = 1 << BTB_INDEX_WIDTH
}