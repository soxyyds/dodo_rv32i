package DODO

import chisel3._
import chisel3.util._
import chisel3.stage._


class Top extends Module{
  val io = IO(new Bundle{
    val pc = Output(UInt(64.W))
    val Inst_A = Input(UInt(32.W))
    val Inst_B = Input(UInt(32.W))
    val DataRam = new RAMHelperIO_2

  })

  val InstFetch   = Module(new InstFetch)
  val InstDecode  = Module(new InstDecode)
  val RegMap      = Module(new RegMap)
  val Dispatch    = Module(new dispatch)
  val RegRead     = Module(new RegRead)
  val Execute     = Module(new Execute)
  val Memory      = Module(new Memory)
  val Commit      = Module(new Commit)

  //io.read_func3 := Memory.io.read_func3// 读取功能码
  io.pc := InstFetch.io.addressout
  InstFetch.io.Inst_In_A := io.Inst_A
  InstFetch.io.Inst_In_B := io.Inst_B


  // pipeline
  InstFetch.io.IFIDA <> InstDecode.io.IFIDA
  InstFetch.io.IFIDB <> InstDecode.io.IFIDB
  InstDecode.io.IDRMA <> RegMap.io.IDRMA
  InstDecode.io.IDRMB <> RegMap.io.IDRMB
  RegMap.io.RMDPA <> Dispatch.io.in_A
  RegMap.io.RMDPB <> Dispatch.io.in_B
  Dispatch.io.out_A <> RegRead.io.DPRRA
  Dispatch.io.out_B <> RegRead.io.DPRRB
  Dispatch.io.out_C  <> RegRead.io.DPRRC
  RegRead.io.RREXA <> Execute.io.RREXA
  RegRead.io.RREXB <> Execute.io.RREXB
  RegRead.io.RREXC <> Execute.io.RREXC
  Execute.io.EXMEM <> Memory.io.EXMEM



  // ReOrder
  Commit.io.EnA <> RegMap.io.RMDPA
  Commit.io.EnB <> RegMap.io.RMDPB
  Commit.io.ReOrderNumA <> RegMap.io.ReOrderNumA
  Commit.io.ReOrderNumB <> RegMap.io.ReOrderNumB
  Commit.io.FinA <> RegRead.io.FinA
  Commit.io.FinB <> RegRead.io.FinB
  Commit.io.FinC <> Execute.io.FinC
  Commit.io.FinD <> Execute.io.FinD
  Commit.io.FinE <> Memory.io.FinE
  Commit.io.CmtA <> InstFetch.io.CmtA
  Commit.io.CmtA <> RegMap.io.CmtA
  Commit.io.CmtB <> RegMap.io.CmtB
  Commit.io.CmtA <> Memory.io.CmtA
  Commit.io.CmtB <> InstFetch.io.CmtB

  // dispatch
  Dispatch.io.regstate <> RegMap.io.PhyRegStates

  // map execute
  RegMap.io.FinA <> RegRead.io.FinA
  RegMap.io.FinB <> RegRead.io.FinB
  RegMap.io.FinC <> Execute.io.FinC
  RegMap.io.FinD <> Execute.io.FinD
  RegMap.io.FinE <> Memory.io.FinE
  RegRead.io.FinC <> Execute.io.FinC
  RegRead.io.FinD <> Execute.io.FinD
  RegRead.io.FinE <> Memory.io.FinE

  // block
  val FetchBlock = (Dispatch.io.fetchblock) || Commit.io.FetchBlock
  FetchBlock <> InstFetch.io.FetchBlock
  FetchBlock <> InstDecode.io.FetchBlock
  FetchBlock <> RegMap.io.FetchBlock

  // rollback
  Commit.io.Rollback <> InstFetch.io.Rollback
  Commit.io.Rollback <> InstDecode.io.Rollback
  Commit.io.Rollback <> RegMap.io.Rollback
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
    val writeEnable = Output(Bool()) // 暴露写使能信号
    val writeAddr = Output(UInt(64.W)) // 暴露写地址
    val writeData = Output(UInt(32.W)) // 暴露写数据

    val mem_writeEnable = Output(Bool()) // 暴露内存写使能信号
    val mem_writeAddr = Output(UInt(64.W)) // 暴露内存写地址
    val mem_readAddr = Output(UInt(64.W)) // 暴露内存读地址
    val mem_writeData = Output(UInt(32.W)) // 暴露内存写数据
    val mem_func3_write = Output(UInt(3.W)) // 暴露内存读地址
    val mem_func3_read = Output(UInt(3.W)) // 暴露内存读功能码
    val mem_rdata = Output(UInt(32.W)) // 暴露内存读数据

  })

  val cpu = Module(new Top)
  val data_memory = Module(new mem(memDepth = 65536, instWidth = 2))

  // 现有连接保持不变
  data_memory.io.if_mem.instAddr := cpu.io.pc
  cpu.io.Inst_A := data_memory.io.mem_id.inst(0)
  cpu.io.Inst_B := data_memory.io.mem_id.inst(1)
  data_memory.io.ex_mem.writeEn := cpu.io.DataRam.data_wen
  io.writeEnable := cpu.io.DataRam.data_wen // 暴露写使能信号
  io.writeAddr := cpu.io.DataRam.data_address // 暴露写地址
  io.writeData := cpu.io.DataRam.data_wdata // 暴露写数据

  data_memory.io.ex_mem.dataAddr_wirte := cpu.io.DataRam.data_address
  data_memory.io.ex_mem.dataAddr_read := cpu.io.DataRam.read_address
  data_memory.io.ex_mem.writeData := cpu.io.DataRam.data_wdata
  cpu.io.DataRam.data_rdata := data_memory.io.mem_lsu.data
  data_memory.io.ex_mem.func3_write := cpu.io.DataRam.func3_write
  data_memory.io.ex_mem.func3_read := cpu.io.DataRam.func3_read // 暴露功能码
  data_memory.io.reset := reset

  // 连接调试信号到模块输出
  io.pc := cpu.io.pc
  io.instA := data_memory.io.mem_id.inst(0)
  io.instB := data_memory.io.mem_id.inst(1)

  io.mem_writeEnable := cpu.io.DataRam.data_wen // 暴露内存写使能信号
  io.mem_writeAddr := cpu.io.DataRam.data_address // 暴露内存写地址
  io.mem_readAddr := cpu.io.DataRam.read_address // 暴露内存读地址
  io.mem_writeData := cpu.io.DataRam.data_wdata // 暴露内存写数据
  io.mem_rdata := data_memory.io.mem_lsu.data // 暴露内存读数据
  io.mem_func3_write := data_memory.io.ex_mem.func3_write // 暴露内存读地址
  io.mem_func3_read := data_memory.io.ex_mem.func3_read // 暴露内存读功能码

}

object ExVerilog extends App {
  (new ChiselStage).emitVerilog(new TopWithMemory(), Array("--target-dir", "generated"))
}