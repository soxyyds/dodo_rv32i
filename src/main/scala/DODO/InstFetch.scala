package DODO

import chisel3._
import chisel3.util._
import DODO.BPU._
import DODO.BPU.Const._

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
    val InstRam = new RAMHelperIO_2

    // **------------------ 新增分支预测器接口 ------------------**
    // ------------------ 新增分支预测器接口 ------------------
    // 分支预测请求(当前 PC 发送给 BPU)当前程序计数器PC的值发送给分支预测器,请求预测该地址是否对应一条分支指令,以及预测的跳转目标。
    val bpLookupPc = Output(UInt(32.W))
    // 分支预测结果(来自 BPU)
    val bpPredTakenA = Input(Bool())//分支预测器返回的 预测结果，表示当前 PC 对应的分支指令是否预测跳转。
    //true.B,预测跳转,Fetch 应更新 PC 为 bpPredTarget。
    //false.B:预测不跳转,Fetch 继续顺序执行(PC+4 或 PC+8)
    val bpPredTargetA = Input(UInt(64.W))//分支预测器返回的 预测跳转目标地址，仅在 bpPredTaken 为真时有效。
    //若预测跳转,Fetch 将 PC 更新为此地址。
    //通常来自 BTB 中存储的历史跳转目标。
    // 双发射分支预测接口
    val bpPredTakenB = Input(Bool())
    val bpPredTargetB = Input(UInt(64.W))
    // 新增：接收RegRead阶段反馈的分支信息。经过IF以保证时序正确
    val bpuBranchA = Input(new BPU.BranchIO)
    val bpuBranchB = Input(new BPU.BranchIO)
    // 新增：输出分支预测index到RegRead
    val bpuBranchA_index = Output(UInt(GHR_WIDTH.W))
    val bpuBranchB_index = Output(UInt(GHR_WIDTH.W))
  })

  // 初始化与预热逻辑
  val Warmup = RegInit(0.U(3.W))
  when(Warmup === 4.U) { Warmup := 4.U }
    .otherwise { Warmup := Warmup + 1.U }
  val ENABLE = Warmup === 4.U && !io.FetchBlock//这里阻塞信号起到了作用

  // ------------------ 分支预测器例化（双发射） ------------------
  val bpA = Module(new BP)
  val bpB = Module(new BP)

  // A通道分支预测
  bpA.io.lookupPc := PC
  bpA.io.branchIO := io.bpuBranchA // RegRead阶段反馈的A通道分支信息
  io.bpPredTakenA := bpA.io.predTaken
  io.bpPredTargetA := bpA.io.predTarget

  // B通道分支预测
  val nextPC_B = PC + 4.U // B通道PC为PC+4
  bpB.io.lookupPc := nextPC_B
  bpB.io.branchIO := io.bpuBranchB
  io.bpPredTakenB := bpB.io.predTaken
  io.bpPredTargetB := bpB.io.predTarget

  // 新增：将分支预测index输出到RegRead
  io.bpuBranchA_index := bpA.io.predIndex
  io.bpuBranchB_index := bpB.io.predIndex

  // ------------------ PC 更新逻辑（支持双发射分支预测，含冲突处理） ------------------
  val PC = RegInit("h0000000080000000".U(64.W))
  // ------------------ PC 更新逻辑（支持分支预测） ------------------
  val PC = RegInit("h0000000080000000".U(64.W))//定义了一个64位的地址寄存器
  val Offset = "h0000000080000000".U(64.W)
  val Unaligned = PC(2) === 1.U// 检查 PC 是否未对齐 8 字节，8字节就是8的倍数，如果为1就是8对齐
//如果是8对齐 就第三位整除,因为要么是8要么是4
  // 实现分支预测的板块，优先级:Commit 反馈 > 分支预测 > 默认顺序执行
  when(io.Rollback) {
    // 分支预测失败时，使用 Commit 提供的正确 PC,因为commit里的跳转具有强制性
    PC := Mux(io.CmtA.jump.Valid, io.CmtA.jump.actTarget, io.CmtA.branch.target)
  }.elsewhen(io.CmtA.jump.Valid) {
    // Commit 阶段的跳转指令（绝对跳转）
    PC := io.CmtA.jump.actTarget
  }.elsewhen(io.CmtA.branch.Valid && (io.CmtA.branch.actTaken).asBool) {
    // Commit 阶段的分支指令（条件分支且实际跳转）
    PC := io.CmtA.branch.target
  }.otherwise {
    when(ENABLE) {
      // 动态分支预测：如果A预测跳转，则更新 PCA 为预测目标
      when(io.bpPredTakenA) {
        // A预测跳转，若B也预测跳转且目标不同，优先A
        PC := io.bpPredTargetA
      }.elsewhen(io.bpPredTakenB) {
        PC := io.bpPredTargetB
      }.otherwise {
        when(Unaligned) {
          PC := PC + 4.U
        } .otherwise {
          PC := PC + 8.U
        }
      }
    }
  }
  // 注：此处A优先，B预测跳转仅在A未跳转时生效。

  // ------------------ 指令存储器访问（保持不变） ------------------
  //在这个部分是获取对应的信息的地方
  io.InstRam.clk := clock
  io.InstRam.inst_gain_en := ENABLE
  io.InstRam.inst_address := Cat(0.U(3.W), (PC - Offset)(63,3))
  io.InstRam.data_address := 0.U
  io.InstRam.data_wdata := 0.U
  io.InstRam.data_rdata := 0.U
  io.InstRam.data_wen := false.B//其他多余的信号需要初始化废弃掉


  // ------------------ 指令拆分与输出逻辑（保持不变） ------------------
  val ValidA = Mux(Unaligned, ENABLE, ENABLE)//enable控制了指令是否需要取
  val ValidB = Mux(Unaligned, false.B, ENABLE)
  val InstA = Mux(Unaligned, io.InstRam.inst_data(1), io.InstRam.inst_data(0))
  val InstB = Mux(Unaligned, 0.U(32.W), io.InstRam.inst_data(1))//这里虽然没有直接受enable控制，但是实际上pc没有动，每次取得的inst还是一样
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
}//在这个地方传给后面instfetch其实是一个指令是否有效（因为有些指令传一个 另一些传两条）
//然后还有指令的具体32位值 还有其指令的64位pc的地址 Valid: Bool, inst: UInt, pc: UInt 这三个信息是从内存mem里面获取的
class RAMHelperIO_2 extends Bundle {
  // 基础时钟
  val clk = Output(Clock())
  // 指令读取端口
  val inst_en = Input(Bool())
  val inst_gain_en    = Output(Bool())//从内存里面获取对应的指令的使能信号
  val inst_address = Output(UInt(64.W))//用于检索的地址
  val inst_data  = Input(Vec(2, UInt(32.W))) // 返回双指令。双指令的riscv指令
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