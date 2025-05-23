package DODO

import chisel3._
import chisel3.util._


class InstFetch extends Module {
  val io = IO(new Bundle {
    // 输出到 Decode 阶段的双指令，这个指令的话是从内存板块里面取出来的
    val IFIDA = Output(new InstCtrlBlock)
    val IFIDB = Output(new InstCtrlBlock)

    // 来自 Commit 阶段的反馈（实际跳转/分支结果）
    val CmtA = Input(new InstCtrlBlock)
    // 流水线控制信号
    val FetchBlock = Input(Bool())  // 阻塞信号(如 Cache Miss)
    val Rollback = Input(Bool())    // 分支预测失败回滚

    // 指令存储器接口
    val addressout = Output(UInt(64.W)) //指令存储器的地址
    val Inst_In_A = Input(UInt(32.W)) //指令存储器返回的指令
    val Inst_In_B = Input(UInt(32.W)) //指令存储器返回的指令


  })

  val Bp = Module(new BranchPredictor) // 分支预测器实例
  Bp.io.if_pc_0 := io.addressout // 传入当前 PC 地址
  Bp.io.if_pc_1 := io.addressout + 4.U // 传入下一个 PC 地址

  Bp.io.ex_pc := io.CmtA.pc
  Bp.io.ex_is_branch := io.CmtA.branch.Valid
  Bp.io.ex_is_taken := io.CmtA.branch.actTaken
  Bp.io.ex_mis_pred := io.Rollback
  Bp.io.ex_target := io.CmtA.branch.target
  Bp.io.ex_pred_index := io.CmtA.bppredIndex
  Bp.io.ex_is_jump := io.CmtA.jump.Valid


  // 初始化与预热逻辑
  val Warmup = RegInit(0.U(3.W))
  when(Warmup === 4.U) { Warmup := 4.U }
    .otherwise { Warmup := Warmup + 1.U }
  val ENABLE = Warmup === 4.U && !io.FetchBlock//这里阻塞信号起到了作用


  // ------------------ PC 更新逻辑（支持双发射分支预测，含冲突处理） ------------------
  val PC = RegInit("h0000000080000000".U(64.W)) // PC 寄存器，初始值为 0x80000000


  val branch_true_target = Wire(UInt(64.W))
  branch_true_target := Mux(io.CmtA.branch.Valid && io.CmtA.branch.actTaken, io.CmtA.branch.target, io.CmtA.pc + 4.U)
  val jump_true_target = Wire(UInt(64.W))
  jump_true_target := io.CmtA.jump.actTarget

  // 实现分支预测的板块，优先级:Commit 反馈 > 分支预测 > 默认顺序执行
  when(io.Rollback) {
    // 分支预测失败时，使用 Commit 提供的正确 PC,因为commit里的跳转具有强制性
    PC := Mux(io.CmtA.jump.Valid, jump_true_target, branch_true_target)
  }.otherwise {
    when(ENABLE) {
      // 动态分支预测：如果A预测跳转，则更新 PCA 为预测目标
      when(Bp.io.pred_taken_0) {
        // A预测跳转，若B也预测跳转且目标不同，优先A
        PC := Bp.io.pred_target_0
      }.elsewhen(Bp.io.pred_taken_1) {
        PC := Bp.io.pred_target_1
      }.otherwise {
          PC := PC + 8.U
        }
      }
    }
  // 注：此处A优先，B预测跳转仅在A未跳转时生效。

  // ------------------ 指令拆分与输出逻辑（保持不变） ------------------
  val ValidA =ENABLE
  val ValidB = Mux(Bp.io.pred_taken_0, false.B, ENABLE)
  val InstA = io.Inst_In_A
  val InstB = Mux(Bp.io.pred_taken_0, false.B, io.Inst_In_B)
  val PCA = PC
  val PCB = PC + 4.U

  io.addressout := PC


  when(io.Rollback) {
    io.IFIDA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.IFIDB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise {
    io.IFIDA := GenICB(ValidA, InstA, PCA,Bp.io.pred_taken_0, Bp.io.pred_index_0, Bp.io.pred_target_0)
    io.IFIDB := GenICB(ValidB, InstB, PCB, Bp.io.pred_taken_1, Bp.io.pred_index_1, Bp.io.pred_target_1)

  }

  //实际上只生成了指令控制块生成函数,构造一个空的指令控制块(InstCtrlBlock),仅填充有效位、指令内容和 PC,其余字段由后续流水线阶段赋值。
  def GenICB(Valid: Bool, inst: UInt, pc: UInt, pred_index:UInt, pred_taken:UInt, pred_target:UInt): InstCtrlBlock = {
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
    ICB.csr_addr := 0.U
    ICB.csr_wdata := 0.U
    //分支预测相关
    ICB.bpPredTaken := pred_taken
    ICB.bpPredTarget := pred_target
    ICB.bppredIndex := pred_index
    ICB
  }
}


class RAMHelperIO_2 extends Bundle {
  //数据读取端口
  // 读取数据，存储器返回的读取的数据，就是说这些load取出来的数据
   // 指令地址（字节地址），输出当前需要读取的指令地址（字节地址）
  // 指令读取使能是memory相关的东西，true的时候需要在指定的地址处返回指令的数据
  // 数据存入端口信号
  //其实大概分为六类 读使能 地址 数据   写使能 地址 数据
  val data_address   = Output(UInt(64.W)) // 数据地址（字节地址）需要存入或者取出的数据的地址
  val data_wen    = Output(Bool())     // 写使能，数据写的信号，决定是否可以存入的信号，这个由cmtable决定
  val data_wdata  = Output(UInt(64.W)) // 写入数据，需要存入存储器的数据
  val func3  = Output(UInt(3.W))  // 字节写掩码
  val data_rdata  = Input(UInt(64.W))//读取的使能信号是受dataAddr 和 func3 有效和wen信号控制的
  // 原子操作标志
}