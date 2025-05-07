package DODO

import chisel3._
import chisel3.util._

// Memory阶段：负责load从内存读数，store准备写数（真正写入等到提交）
class Memory extends Module {
  val io = IO(new Bundle {
    val EXMEM = Input(new InstCtrlBlock)  // 来自EX阶段的指令
    val FinE  = Output(new InstCtrlBlock) // 传给下一阶段的结果
    val CmtA  = Input(new InstCtrlBlock)  // 来自提交阶段的store信息

    val ForwardLoad  = Output(new LoadIssue)  // 给load做前递的数据
    val ForwardStore = Input(new StoreIssue)  // 从ROB来的store前递信息

    val Rollback = Input(Bool())  // 回滚信号
    val DataRam  = new RAMHelperIO // 连接到Data RAM
  })

  // 把EXMEM信号寄存，作为这一周期使用
  val INST = RegNext(io.EXMEM)

  // 地址偏移：默认是映射到虚拟内存空间
  val Offset = "h0000000080000000".U(64.W)

  // RAM访问控制：load阶段直接读；store等提交再写
  io.DataRam.clk   := clock
  io.DataRam.en    := INST.load.Valid || (io.CmtA.Valid && io.CmtA.store.Valid)
  io.DataRam.rIdx  := Cat(0.U(3.W), (INST.load.addr - Offset)(63, 3))
  io.DataRam.wIdx  := Cat(0.U(3.W), (io.CmtA.store.addr - Offset)(63, 3))
  io.DataRam.wdata := io.CmtA.store.data
  io.DataRam.wmask := io.CmtA.store.mask
  io.DataRam.wen   := io.CmtA.Valid && io.CmtA.store.Valid

  // load会查ROB有没有尚未提交的store写入（前递）
  io.ForwardLoad := Mux(INST.isa.Lclass, io.FinE.load, 0.U.asTypeOf(new LoadIssue))

  // 如果有前递store，就选用前递数据，否则为0
  val wdata = Mux(io.ForwardStore.Valid, io.ForwardStore.data, 0.U)
  val wmask = Mux(io.ForwardStore.Valid, io.ForwardStore.mask, 0.U)

  // 拼接前递数据和内存数据：用mask控制混合哪些字节
  val d_data = (io.DataRam.rdata & ~wmask) | (wdata & wmask)

  // 按照地址低位，逐级选择字节/半字/字
  val w_data = Mux(INST.load.addr(2), d_data(63, 32), d_data(31, 0))
  val h_data = Mux(INST.load.addr(1), w_data(31, 16), w_data(15, 0))
  val b_data = Mux(INST.load.addr(0), h_data(15, 8),  h_data(7, 0))

  // 处理不同load指令（有符号/无符号）
  val LD_data  = SignExt(INST.isa.LD.asUInt, 64)  & d_data
  val LW_data  = SignExt(INST.isa.LW.asUInt, 64)  & SignExt(w_data, 64)
  val LH_data  = SignExt(INST.isa.LH.asUInt, 64)  & SignExt(h_data, 64)
  val LB_data  = SignExt(INST.isa.LB.asUInt, 64)  & SignExt(b_data, 64)
  val LWU_data = SignExt(INST.isa.LWU.asUInt, 64) & ZeroExt(w_data, 64)
  val LHU_data = SignExt(INST.isa.LHU.asUInt, 64) & ZeroExt(h_data, 64)
  val LBU_data = SignExt(INST.isa.LBU.asUInt, 64) & ZeroExt(b_data, 64)

  // 合并最终load数据（只有一种load类型会生效）
  val LoadData = LD_data | LW_data | LH_data | LB_data | LWU_data | LHU_data | LBU_data

  // 如果发生回滚，FinE置空；否则更新为正常load/store结果
  when(io.Rollback) {
    io.FinE := 0.U.asTypeOf(new InstCtrlBlock)
  }.otherwise {
    io.FinE := GenFin(INST.isa.Lclass || INST.isa.Sclass, LoadData, INST)
  }

  // 封装生成 FinE 的函数
  def GenFin(finish: Bool, LoadData: UInt, EXMEM: InstCtrlBlock): InstCtrlBlock = {
    val out = Wire(new InstCtrlBlock)
    out := EXMEM
    out.finish := finish
    out.wbdata := LoadData

    // load返回的数据附带上
    out.load.Valid := EXMEM.load.Valid
    out.load.addr  := EXMEM.load.addr
    out.load.data  := LoadData
    out.load.Ready := EXMEM.load.Ready

    out
  }
}
