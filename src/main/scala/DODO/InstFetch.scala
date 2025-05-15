package DODO

import chisel3._
import chisel3.util._
import DODO.BPU._  // 导入分支预测器相关模块

class InstFetch extends Module {
  val io = IO(new Bundle {
    // 输出到 Decode 阶段的双指令
    val IFIDA = Output(new InstCtrlBlock)
    val IFIDB = Output(new InstCtrlBlock)

    // 来自 Commit 阶段的反馈（实际跳转/分支结果）
    val CmtA = Input(new InstCtrlBlock)
    // 流水线控制信号
    val FetchBlock = Input(Bool())  // 阻塞信号(如 Cache Miss)
    val Rollback = Input(Bool())    // 分支预测失败回滚

    // 指令存储器接口
    val InstRam = new RAMHelperIO

    // **------------------ 新增分支预测器接口 ------------------**
    // 分支预测请求(当前 PC 发送给 BPU)
    val bpLookupPcA = Output(UInt(64.W))
    val bpLookupPcB = Output(UInt(64.W))
    // 分支预测结果(来自 BPU)
    val bpPredTakenA = Input(Bool())
    val bpPredTargetA = Input(UInt(64.W))
    val bpPredTakenB = Input(Bool())
    val bpPredTargetB = Input(UInt(64.W))
  })

  // 初始化与预热逻辑(保持不变)
  val Warmup = RegInit(0.U(3.W))
  when(Warmup === 4.U) { Warmup := 4.U }
    .otherwise { Warmup := Warmup + 1.U }
  val ENABLE = Warmup === 4.U && !io.FetchBlock

  // ------------------ PC 更新逻辑（支持分支预测） ------------------
  val PC = RegInit("h0000000080000000".U(64.W))
  val Offset = "h0000000080000000".U(64.W)
  val Unaligned = PC(2) === 1.U  // 检查 PC 是否未对齐 8 字节

  // 实现分支预测的板块，优先级:Commit 反馈 > 分支预测 > 默认顺序执行
  when(io.Rollback) {
    // 分支预测失败时，使用 Commit 提供的正确 PC,因为commit里的跳转具有强制性
    PC := Mux(io.CmtA.jump.Valid, io.CmtA.jump.actTarget, io.CmtA.branch.target)
  }.elsewhen(io.CmtA.jump.Valid) {
    // Commit 阶段的跳转指令（绝对跳转）
    PC := io.CmtA.jump.actTarget
  }.elsewhen(io.CmtA.branch.Valid && io.CmtA.branch.actTaken) {
    // Commit 阶段的分支指令（条件分支且实际跳转）
    PC := io.CmtA.branch.target
  }.otherwise {
    when(ENABLE) {
      // 动态分支预测：如果预测跳转，则更新 PC 为预测目标
      when(io.bpPredTakenA) {
        PC := io.bpPredTargetA
      }.otherwise {
        // 默认顺序执行
        when(Unaligned) { PC := PC + 4.U }//未对齐+4
          .otherwise { PC := PC + 8.U }///对齐+8
      }
    }
  }

  // ------------------ 指令存储器访问（保持不变） ------------------
  io.InstRam.clk := clock
  io.InstRam.en := ENABLE
  io.InstRam.rIdx := Cat(0.U(3.W), (PC - Offset)(63,3))
  io.InstRam.wIdx := 0.U
  io.InstRam.wdata := 0.U
  io.InstRam.wmask := 0.U
  io.InstRam.wen := false.B

  // ------------------ 分支预测器接口连接 ------------------
  io.bpLookupPcA := PCA
  io.bpLookupPcB := PCB

  // ------------------ 指令拆分与输出逻辑（保持不变） ------------------
  val ValidA = Mux(Unaligned, ENABLE, ENABLE)
  val ValidB = Mux(Unaligned, false.B, ENABLE)
  val InstA = Mux(Unaligned, io.InstRam.rdata(63,32), io.InstRam.rdata(31,0))
  val InstB = Mux(Unaligned, 0.U(32.W), io.InstRam.rdata(63,32))
  val PCA = Mux(Unaligned, PC, PC)
  val PCB = Mux(Unaligned, 0.U(64.W), PC + 4.U)

  when(io.Rollback) {
    io.IFIDA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.IFIDB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise {
    io.IFIDA := GenICB(ValidA, InstA, PCA)
    io.IFIDB := GenICB(ValidB, InstB, PCB)
  }

  //实际上只生成了指令控制块生成函数,构造一个空的指令控制块(InstCtrlBlock),仅填充有效位、指令内容和 PC,其余字段由后续流水线阶段赋值。
  def GenICB(Valid: Bool, inst: UInt, pc: UInt): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB.Valid := Valid//有效位
    ICB.inst := inst//指令内容
    ICB.pc := pc//PC
    ICB.isa := WireInit(0.U.asTypeOf(new ISA()))
    ICB.finish := false.B
    ICB.reOrderNum := 0.U
    ICB.regdes := 0.U
    ICB.regsrc1 := 0.U
    ICB.regsrc2 := 0.U
    ICB.pregsrc1 := 0.U
    ICB.pregsrc2 := 0.U
    ICB.pregdes := 0.U
    ICB.cmtdes := 0.U
    ICB.src1 := 0.U
    ICB.src2 := 0.U
    ICB.imm := WireInit(0.U.asTypeOf(new IMM()))
    ICB.wbdata := 0.U
    ICB.jump := WireInit(0.U.asTypeOf(new JumpIssue()))
    ICB.branch := WireInit(0.U.asTypeOf(new BranchIssue()))
    ICB.load := WireInit(0.U.asTypeOf(new LoadIssue()))
    ICB.store := WireInit(0.U.asTypeOf(new StoreIssue()))
    ICB
  }
}

class RAMHelperIO extends Bundle{
  val clk = Output(Clock())//时钟
  val en = Output(Bool())//使能
  val rIdx = Output(UInt(64.W))//读地址索引(important)
  val rdata = Input(UInt(64.W))//读数据(important)
  //写端口（取指无需使用）
  val wIdx = Output(UInt(64.W))
  val wdata = Output(UInt(64.W))
  val wmask = Output(UInt(64.W))
  val wen = Output(Bool())
}