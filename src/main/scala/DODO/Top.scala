package DODO

import chisel3._
import chisel3.util._
import BPU._

class Top extends Module{
  val io = IO(new Bundle{
    val InstRam = new RAMHelperIO
    val DataRam = new RAMHelperIO
    //    val uart = new UARTIO
  })

  // Module
  val BPMachine   = Module(new BP)
  val InstFetch   = Module(new InstFetch)
  val InstDecode  = Module(new InstDecode)
  val RegMap      = Module(new RegMap)
  val Dispatch    = Module(new dispatch)
  val RegRead     = Module(new RegRead)
  val Execute     = Module(new Execute)
  val Memory      = Module(new Memory)
  val Commit      = Module(new Commit)

  // pipeline
  InstFetch.io.IFIDA <> InstDecode.io.IFIDA
  InstFetch.io.IFIDB <> InstDecode.io.IFIDB
  InstDecode.io.IDRMA <> RegMap.io.in_A //RegMap的输入改名了
  InstDecode.io.IDRMB <> RegMap.io.in_B
  RegMap.io.out_A <> Dispatch.io.in_A //RegMap的输出改名了
  RegMap.io.out_B <> Dispatch.io.in_B //dispatch的输入改名了
  Dispatch.io.out_A <> RegRead.io.DPRRA //dispatch的输出改名了
  Dispatch.io.out_B <> RegRead.io.DPRRB
  Dispatch.io.out_C  <> RegRead.io.DPRRC
  RegRead.io.RREXA <> Execute.io.RREXA
  RegRead.io.RREXB <> Execute.io.RREXB
  RegRead.io.RREXC <> Execute.io.RREXC
  Execute.io.EXMEM <> Memory.io.EXMEM

  // 分支预测相关信号连接（双发射闭环）
  // InstFetch <-> RegRead
  RegRead.io.bpuBranchA_index := InstFetch.io.bpuBranchA_index
  RegRead.io.bpuBranchB_index := InstFetch.io.bpuBranchB_index
  InstFetch.io.bpuBranchA := RegRead.io.bpuBranchA
  InstFetch.io.bpuBranchB := RegRead.io.bpuBranchB

  // ReOrder
  Commit.io.EnA <> RegMap.io.out_A
  Commit.io.EnB <> RegMap.io.out_B
  Commit.io.ReOrderNumA <> RegMap.io.num_A //ReorderNumA改成了num_A
  Commit.io.ReOrderNumB <> RegMap.io.num_B
  Commit.io.FinA <> RegRead.io.FinA
  Commit.io.FinB <> RegRead.io.FinB
  Commit.io.FinC <> Execute.io.FinC
  Commit.io.FinD <> Execute.io.FinD
  Commit.io.FinE <> Memory.io.FinE
  Commit.io.CmtA <> InstFetch.io.CmtA
  Commit.io.CmtA <> RegMap.io.cmt_A //CmtA改成了cmt_A
  Commit.io.CmtB <> RegMap.io.cmt_B
  Commit.io.CmtA <> Memory.io.CmtA

  // dispatch
  Dispatch.io.regstate <> RegMap.io.regstate  //PhyRegStates改成了regstate

  // map execute
  RegMap.io.fin_A <> RegRead.io.FinA //RegMap中FinA改成了fin_A
  RegMap.io.fin_B <> RegRead.io.FinB
  RegMap.io.fin_C <> Execute.io.FinC
  RegMap.io.fin_D <> Execute.io.FinD
  RegMap.io.fin_E <> Memory.io.FinE
  RegRead.io.FinC <> Execute.io.FinC
  RegRead.io.FinD <> Execute.io.FinD
  RegRead.io.FinE <> Memory.io.FinE

  // block
  val FetchBlock = Dispatch.io.enable || Commit.io.FetchBlock
  FetchBlock <> InstFetch.io.FetchBlock
  FetchBlock <> InstDecode.io.FetchBlock
  FetchBlock <> RegMap.io.enable //RegMap和Dispatch的FetchBlock改成了enable

  // rollback
  Commit.io.Rollback <> InstFetch.io.Rollback
  Commit.io.Rollback <> InstDecode.io.Rollback
  Commit.io.Rollback <> RegMap.io.rollback //Rollback改成了rollback
  Commit.io.Rollback <> Dispatch.io.rollback
  Commit.io.Rollback <> RegRead.io.Rollback
  Commit.io.Rollback <> Execute.io.Rollback
  Commit.io.Rollback <> Memory.io.Rollback

  // load forward
  Commit.io.ForwardLoad <> Memory.io.ForwardLoad
  Commit.io.ForwardStore <> Memory.io.ForwardStore

  // IO
  io.InstRam <> InstFetch.io.InstRam
  io.DataRam <> Memory.io.DataRam

  // CSR寄存器定义
  val mcycle = RegInit(0.U(64.W))
  mcycle := mcycle + 1.U  // 自动递增计数器（每个时钟周期+1）

  // 提交阶段A处理
  when (Commit.io.CmtA.Valid && Commit.io.CmtA.isa.CSRRW) {
    when (Commit.io.CmtA.csr_addr === 0xB00.U) {  // mcycle地址匹配
      mcycle := Commit.io.CmtA.wbdata            // 写回数据直接更新计数器
    }
  }

  // 提交阶段B处理（双提交通道设计）
  when (Commit.io.CmtB.Valid && Commit.io.CmtB.isa.CSRRW) {
    when (Commit.io.CmtB.csr_addr === 0xB00.U) {  // 统一地址匹配逻辑
      mcycle := Commit.io.CmtB.wbdata
    }
  }

  // difftest
  //  val CmtA = Commit.io.CmtA
  //  val CmtB = Commit.io.CmtB
  //  val isPrint = CmtA.inst(6,0) === "h7b".U(7.W)
  //  val isHalt = CmtA.inst(6,0) === "h6b".U(7.W)
  //  val cycleCnt = RegInit(0.U(64.W))
  //  val instrCnt = RegInit(0.U(64.W))
  //
  //  val Aready = CmtA.Valid && CmtA.finish
  //  val Bready = CmtB.Valid && CmtB.finish
  //  cycleCnt := cycleCnt + 1.U
  //  instrCnt := instrCnt + Aready.asUInt + Bready.asUInt
  //
  //  val InstrCommitA = Module(new DifftestInstrCommit)
  //  val InstrCommitB = Module(new DifftestInstrCommit)
  //  val ArchIntRegState = Module(new DifftestArchIntRegState)
  //  val CSRState = Module(new DifftestCSRState)
  //  val TrapEvent = Module(new DifftestTrapEvent)
  //  val ArchFpRegState = Module(new DifftestArchFpRegState)
  //  val ArchEvent = Module(new DifftestArchEvent)
  //
  //  InstrCommitA.io.clock := clock
  //  InstrCommitA.io.coreid := 0.U
  //  InstrCommitA.io.index := 0.U
  //  InstrCommitA.io.valid := RegNext(Aready)
  //  InstrCommitA.io.pc := RegNext(CmtA.pc)
  //  InstrCommitA.io.instr := RegNext(CmtA.inst)
  //  InstrCommitA.io.skip := RegNext(isPrint)
  //  InstrCommitA.io.isRVC := false.B
  //  InstrCommitA.io.scFailed := false.B
  //  InstrCommitA.io.wen := RegNext(CmtA.pregdes===0.U)
  //  InstrCommitA.io.wdata := RegNext(CmtA.wbdata)
  //  InstrCommitA.io.wdest := RegNext(CmtA.pregdes)
  //  InstrCommitA.io.special := 0.U
  //
  //  InstrCommitB.io.clock := clock
  //  InstrCommitB.io.coreid := 0.U
  //  InstrCommitB.io.index := 1.U
  //  InstrCommitB.io.valid := RegNext(Bready)
  //  InstrCommitB.io.pc := RegNext(CmtB.pc)
  //  InstrCommitB.io.instr := RegNext(CmtB.inst)
  //  InstrCommitB.io.skip := false.B
  //  InstrCommitB.io.isRVC := false.B
  //  InstrCommitB.io.scFailed := false.B
  //  InstrCommitB.io.wen := RegNext(CmtB.pregdes===0.U)
  //  InstrCommitB.io.wdata := RegNext(CmtB.wbdata)
  //  InstrCommitB.io.wdest := RegNext(CmtB.pregdes)
  //  InstrCommitB.io.special := 0.U
  //
  //  TrapEvent.io.clock := clock
  //  TrapEvent.io.coreid := 0.U
  //  TrapEvent.io.valid := RegNext(isHalt)
  //  TrapEvent.io.code := RegMap.io.ArchRegValues(10)(7,0)
  //  TrapEvent.io.pc := RegNext(CmtA.pc)
  //  TrapEvent.io.cycleCnt := cycleCnt
  //  TrapEvent.io.instrCnt := instrCnt
  //
  //  io.uart.in.valid := false.B
  //  io.uart.out.valid := isPrint
  //  io.uart.out.ch := RegMap.io.ArchRegValues(10)
  //
  //  CSRState.io.clock := clock
  //  CSRState.io.coreid := 0.U
  //  CSRState.io.mstatus := 0.U
  //  CSRState.io.mcause := 0.U
  //  CSRState.io.mepc := 0.U
  //  CSRState.io.sstatus := 0.U
  //  CSRState.io.scause := 0.U
  //  CSRState.io.sepc := 0.U
  //  CSRState.io.satp := 0.U
  //  CSRState.io.mip := 0.U
  //  CSRState.io.mie := 0.U
  //  CSRState.io.mscratch := 0.U
  //  CSRState.io.sscratch := 0.U
  //  CSRState.io.mideleg := 0.U
  //  CSRState.io.medeleg := 0.U
  //  CSRState.io.mtval := 0.U
  //  CSRState.io.stval := 0.U
  //  CSRState.io.mtvec := 0.U
  //  CSRState.io.stvec := 0.U
  //  CSRState.io.priviledgeMode := 3.U
  //
  //  ArchEvent.io.clock := clock
  //  ArchEvent.io.coreid := 0.U
  //  ArchEvent.io.intrNO := 0.U
  //  ArchEvent.io.cause := 0.U
  //  ArchEvent.io.exceptionPC := 0.U
  //  ArchEvent.io.exceptionInst := 0.U
  //
  //  ArchIntRegState.io.clock := clock
  //  ArchIntRegState.io.coreid := 0.U
  //  ArchIntRegState.io.gpr := RegMap.io.ArchRegValues
  //  ArchFpRegState.io.clock := clock
  //  ArchFpRegState.io.coreid := 0.U
  //  ArchFpRegState.io.fpr := RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
}