package DODO

import chisel3._
import chisel3.util._


class Top extends Module{
  val io = IO(new Bundle{
    val pc = Output(UInt(64.W))
    val Inst_A = Input(UInt(32.W))
    val Inst_B = Input(UInt(32.W))
    val DataRam = new RAMHelperIO_2
    //    val uart = new UARTIO
  })

  // Module
  val InstFetch   = Module(new InstFetch)
  val InstDecode  = Module(new InstDecode)
  val RegMap      = Module(new RegMap)
  val Dispatch    = Module(new dispatch)
  val RegRead     = Module(new RegRead)
  val Execute     = Module(new Execute)
  val Memory      = Module(new Memory)
  val Commit      = Module(new Commit)

  io.pc := InstFetch.io.addressout
  InstFetch.io.Inst_In_A := io.Inst_A
  InstFetch.io.Inst_In_B := io.Inst_B


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
}

class TopWithMemory extends Module {
  val io = IO(new Bundle {
    // 新增调试输出端口
    val pc = Output(UInt(64.W))        // 暴露PC值
    val instA = Output(UInt(32.W))     // 暴露第一条指令
    val instB = Output(UInt(32.W))     // 暴露第二条指令
  })

  val cpu = Module(new Top)
  val data_memory = Module(new mem(memDepth = 1024, instWidth = 2))

  // 现有连接保持不变
  data_memory.io.if_mem.instAddr := cpu.io.pc
  cpu.io.Inst_A := data_memory.io.mem_id.inst(0)
  cpu.io.Inst_B := data_memory.io.mem_id.inst(1)
  data_memory.io.ex_mem.writeEn := cpu.io.DataRam.data_wen
  data_memory.io.ex_mem.dataAddr := cpu.io.DataRam.data_address
  data_memory.io.ex_mem.writeData := cpu.io.DataRam.data_wdata
  cpu.io.DataRam.data_rdata := data_memory.io.mem_lsu.data
  data_memory.io.ex_mem.func3 := cpu.io.DataRam.func3
  data_memory.io.reset := reset

  // 连接调试信号到模块输出
  io.pc := cpu.io.pc
  io.instA := data_memory.io.mem_id.inst(0)
  io.instB := data_memory.io.mem_id.inst(1)
}