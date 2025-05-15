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

class BranchCommitIO extends Bundle {
  val branch    = Bool()              // branch
  val jump      = Bool()              // jal or jalr
  val actTaken  = Bool()              // taken or not
  val index     = UInt(GHR_WIDTH.W)   // last index of PHT
  val pc        = UInt(ADDR_WIDTH.W)  // last instruction PC
  val target    = UInt(ADDR_WIDTH.W)  // last branch target
}

class BP extends Module {
  val io = IO(new Bundle {
    // 双通道分支信息输入
    val branchIOA    = Input(new BranchIO)
    val branchIOB    = Input(new BranchIO)
    val branchCommitA = Input(new BranchCommitIO)
    val branchCommitB = Input(new BranchCommitIO)
    // 双通道预测接口
    val lookupPcA    = Input(UInt(ADDR_WIDTH.W))
    val lookupPcB    = Input(UInt(ADDR_WIDTH.W))
    val predTakenA   = Output(Bool())
    val predTargetA  = Output(UInt(ADDR_WIDTH.W))
    val predIndexA   = Output(UInt(GHR_WIDTH.W))
    val predTakenB   = Output(Bool())
    val predTargetB  = Output(UInt(ADDR_WIDTH.W))
    val predIndexB   = Output(UInt(GHR_WIDTH.W))
  })

  // 双通道GHR/PHT/BTB实例
  val ghrA = Module(new GHR)
  val ghrB = Module(new GHR)
  val phtA = Module(new PHT)
  val phtB = Module(new PHT)
  val btbA = Module(new BTB)
  val btbB = Module(new BTB)

  // 优先用Commit阶段真实分支结果更新GHR/PHT
  // A通道
  val commitA_valid = io.branchCommitA.branch || io.branchCommitA.jump
  val updateA_branch = Mux(commitA_valid, io.branchCommitA.branch, io.branchIOA.branch)
  val updateA_taken  = Mux(commitA_valid, io.branchCommitA.actTaken, io.branchIOA.taken)
  val updateA_index  = Mux(commitA_valid, io.branchCommitA.index, io.branchIOA.index)

  // B通道
  val commitB_valid = io.branchCommitB.branch || io.branchCommitB.jump
  val updateB_branch = Mux(commitB_valid, io.branchCommitB.branch, io.branchIOB.branch)
  val updateB_taken  = Mux(commitB_valid, io.branchCommitB.actTaken, io.branchIOB.taken)
  val updateB_index  = Mux(commitB_valid, io.branchCommitB.index, io.branchIOB.index)

  // GHR/PHT更新
  ghrA.io.branch := updateA_branch
  ghrA.io.taken  := updateA_taken
  val indexA = io.lookupPcA(GHR_WIDTH + ADDR_ALIGN_WIDTH - 1, ADDR_ALIGN_WIDTH) ^ ghrA.io.ghr
  phtA.io.lastBranch := updateA_branch
  phtA.io.lastTaken  := updateA_taken
  phtA.io.lastIndex  := updateA_index
  phtA.io.index      := indexA
  btbA.io.branch   := io.branchIOA.branch
  btbA.io.jump     := io.branchIOA.jump
  btbA.io.pc       := io.branchIOA.pc
  btbA.io.target   := io.branchIOA.target
  btbA.io.lookupPc := io.lookupPcA

  ghrB.io.branch := updateB_branch
  ghrB.io.taken  := updateB_taken
  val indexB = io.lookupPcB(GHR_WIDTH + ADDR_ALIGN_WIDTH - 1, ADDR_ALIGN_WIDTH) ^ ghrB.io.ghr
  phtB.io.lastBranch := updateB_branch
  phtB.io.lastTaken  := updateB_taken
  phtB.io.lastIndex  := updateB_index
  phtB.io.index      := indexB
  btbB.io.branch   := io.branchIOB.branch
  btbB.io.jump     := io.branchIOB.jump
  btbB.io.pc       := io.branchIOB.pc
  btbB.io.target   := io.branchIOB.target
  btbB.io.lookupPc := io.lookupPcB

  // 输出A/B通道预测
  io.predTakenA  := btbA.io.lookupBranch && (phtA.io.taken || btbA.io.lookupJump)
  io.predTargetA := btbA.io.lookupTarget
  io.predIndexA  := indexA

  io.predTakenB  := btbB.io.lookupBranch && (phtB.io.taken || btbB.io.lookupJump)
  io.predTargetB := btbB.io.lookupTarget
  io.predIndexB  := indexB
}


  // Constant Definition
object Const {
  val ADDR_WIDTH        = 64
  val ADDR_ALIGN_WIDTH  = log2Ceil(ADDR_WIDTH / 8)
  val GHR_WIDTH         = 5
  val PHT_SIZE          = 1 << GHR_WIDTH
  val BTB_INDEX_WIDTH   = 8
  val BTB_PC_WIDTH      = ADDR_WIDTH - BTB_INDEX_WIDTH - ADDR_ALIGN_WIDTH
  val BTB_TARGET_WIDTH  = ADDR_WIDTH - ADDR_ALIGN_WIDTH
  val BTB_SIZE          = 1 << BTB_INDEX_WIDTH
}

