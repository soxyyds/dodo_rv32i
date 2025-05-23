package DODO


import chisel3._
import chisel3.util._

class BranchPredictorIO extends Bundle {
  // 输入：从EX阶段传来的实际跳转结果
  val ex_is_branch = Input(Bool())
  val ex_is_taken = Input(Bool())
  val ex_mis_pred = Input(Bool())
  val ex_pc = Input(UInt(32.W))
  val ex_target = Input(UInt(32.W))
  val ex_pred_index = Input(UInt(10.W))  // 减小到10位
  val ex_is_jump = Input(Bool())

  // 输入：Fetch阶段的两条指令PC
  val if_pc_0 = Input(UInt(32.W))
  val if_pc_1 = Input(UInt(32.W))

  // 输出：预测结果
  val pred_taken_0 = Output(Bool())
  val pred_target_0 = Output(UInt(32.W))
  val pred_index_0 = Output(UInt(10.W))  // 减小到10位
  val pred_taken_1 = Output(Bool())
  val pred_target_1 = Output(UInt(32.W))
  val pred_index_1 = Output(UInt(10.W))  // 减小到10位
}

class BranchPredictor extends Module {
  val io = IO(new BranchPredictorIO)

  // 简化配置参数
  val BTB_SIZE = 256                // 保持不变
  val GHR_WIDTH = 4                 // 从2位增加到4位
  val PHT_SIZE = 1024               // 从256增加到1024
  val ADDR_ALIGN_WIDTH = 2

  // BTB表设计
  class BTBEntry extends Bundle {
    val valid = Bool()
    val tag = UInt((32 - log2Ceil(BTB_SIZE) - ADDR_ALIGN_WIDTH).W)
    val target = UInt(32.W)
    val is_jump = Bool()
  }

  // BTB实现
  class BTB extends Module {
    val io = IO(new Bundle {
      val pc_0 = Input(UInt(32.W))
      val pc_1 = Input(UInt(32.W))

      val hit_0 = Output(Bool())
      val target_0 = Output(UInt(32.W))
      val is_jump_0 = Output(Bool())
      val hit_1 = Output(Bool())
      val target_1 = Output(UInt(32.W))
      val is_jump_1 = Output(Bool())

      val update = Input(Bool())
      val update_pc = Input(UInt(32.W))
      val update_target = Input(UInt(32.W))
      val update_is_jump = Input(Bool())
    })

    val btbEntries = RegInit(VecInit(Seq.fill(BTB_SIZE)(0.U.asTypeOf(new BTBEntry))))

    def getIndex(pc: UInt): UInt = {
      pc(log2Ceil(BTB_SIZE) + ADDR_ALIGN_WIDTH - 1, ADDR_ALIGN_WIDTH)
    }

    def getTag(pc: UInt): UInt = {
      pc(31, log2Ceil(BTB_SIZE) + ADDR_ALIGN_WIDTH)
    }

    val index_0 = getIndex(io.pc_0)
    val tag_0 = getTag(io.pc_0)
    val entry_0 = btbEntries(index_0)

    val index_1 = getIndex(io.pc_1)
    val tag_1 = getTag(io.pc_1)
    val entry_1 = btbEntries(index_1)

    io.hit_0 := entry_0.valid && entry_0.tag === tag_0
    io.target_0 := entry_0.target
    io.is_jump_0 := entry_0.is_jump

    io.hit_1 := entry_1.valid && entry_1.tag === tag_1
    io.target_1 := entry_1.target
    io.is_jump_1 := entry_1.is_jump

    when(io.update) {
      val update_index = getIndex(io.update_pc)
      val update_tag = getTag(io.update_pc)

      val new_entry = Wire(new BTBEntry)
      new_entry.valid := true.B
      new_entry.tag := update_tag
      new_entry.target := io.update_target
      new_entry.is_jump := io.update_is_jump

      btbEntries(update_index) := new_entry
    }
  }

  // 简化GHR实现
  class GHR extends Module {
    val io = IO(new Bundle {
      val ghr = Output(UInt(GHR_WIDTH.W))
      val update = Input(Bool())
      val taken = Input(Bool())
    })

    val ghr_reg = RegInit(0.U(GHR_WIDTH.W))
    io.ghr := ghr_reg

    // GHR类中
    when(io.update) {
      // 修正为：
      ghr_reg := Cat(ghr_reg(GHR_WIDTH-2, 0), io.taken)
    }
  }

  // PHT实现
  class PHT extends Module {
    val io = IO(new Bundle {
      val index_0 = Input(UInt(log2Ceil(PHT_SIZE).W))
      val index_1 = Input(UInt(log2Ceil(PHT_SIZE).W))

      val taken_0 = Output(Bool())
      val taken_1 = Output(Bool())

      val update = Input(Bool())
      val update_index = Input(UInt(log2Ceil(PHT_SIZE).W))
      val update_taken = Input(Bool())
    })

    val pht = RegInit(VecInit(Seq.fill(PHT_SIZE)(0.U(2.W))))

    io.taken_0 := pht(io.index_0)(1)
    io.taken_1 := pht(io.index_1)(1)

    when(io.update) {
      val counter = pht(io.update_index)
      when(io.update_taken) {
        when(counter =/= 3.U) {
          pht(io.update_index) := counter + 1.U
        }
      }.otherwise {
        when(counter =/= 0.U) {
          pht(io.update_index) := counter - 1.U
        }
      }
    }
  }

  val btb = Module(new BTB)
  val ghr = Module(new GHR)
  val pht = Module(new PHT)

  // Wire定义修改
  val index_0 = Wire(UInt(10.W))  // 10.W
  val index_1 = Wire(UInt(10.W))  // 10.W

  // 确保从PC提取足够的信息位
  val pc_hash_0 = io.if_pc_0(7, 2) ^ io.if_pc_0(13, 8)  // 提取6位
  val pc_hash_1 = io.if_pc_1(7, 2) ^ io.if_pc_1(13, 8)  // 提取6位
  index_0 := Cat(pc_hash_0, ghr.io.ghr)  // 6位PC + 4位GHR = 10位
  index_1 := Cat(pc_hash_1, ghr.io.ghr)  // 6位PC + 4位GHR = 10位

  btb.io.pc_0 := io.if_pc_0
  btb.io.pc_1 := io.if_pc_1

  pht.io.index_0 := index_0
  pht.io.index_1 := index_1

  io.pred_taken_0 := btb.io.hit_0 && (pht.io.taken_0 || btb.io.is_jump_0)
  io.pred_target_0 := btb.io.target_0
  io.pred_index_0 := index_0

  io.pred_taken_1 := btb.io.hit_1 && (pht.io.taken_1 || btb.io.is_jump_1)
  io.pred_target_1 := btb.io.target_1
  io.pred_index_1 := index_1


  btb.io.update := io.ex_mis_pred // 仅在预测错误时更新BTB
  btb.io.update_pc := io.ex_pc
  btb.io.update_target := io.ex_target
  btb.io.update_is_jump := io.ex_is_jump

  ghr.io.update := io.ex_is_branch
  ghr.io.taken := io.ex_is_taken

  pht.io.update := io.ex_is_branch
  pht.io.update_index := io.ex_pred_index
  pht.io.update_taken := io.ex_is_taken
}

// 添加Verilog生成对象
object BranchPredictorVerilog extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(
    new BranchPredictor(),
    args = Array(
      "-o", "branch_predictor.v",  // 输出文件名
      "--target-dir", "generated/branch_predictor"
    )
  )
}

