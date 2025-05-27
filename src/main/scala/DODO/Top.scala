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
    //test
    val fetch_inst_A = Output(new InstCtrlBlock)
    val fetch_inst_B = Output(new InstCtrlBlock)
    val map_inst_A = Output(new InstCtrlBlock)
    val map_inst_B = Output(new InstCtrlBlock)
    val dispatch_inst_A = Output(new InstCtrlBlock)
    val dispatch_inst_B = Output(new InstCtrlBlock)
    val dispatch_inst_C = Output(new InstCtrlBlock)
    val fetchBlock = Output (Bool())
    val decode_inst_A = Output(new InstCtrlBlock)
    val decode_inst_B = Output(new InstCtrlBlock)
    val read_inst_A = Output(new InstCtrlBlock)
    val read_inst_B = Output(new InstCtrlBlock)
 //   val exe_inst_A = Output(new InstCtrlBlock)
 //   val exe_inst_B = Output(new InstCtrlBlock)
 //   val memory_inst_A = Output(new InstCtrlBlock)
 //   val memory_inst_B = Output(new InstCtrlBlock)
    val com_inst_A = Output(new InstCtrlBlock)
    val com_inst_B = Output(new InstCtrlBlock)
    val fin_A = Output(new InstCtrlBlock)
    val fin_B = Output(new InstCtrlBlock)
    val fin_C = Output(new InstCtrlBlock)
    val fin_D = Output(new InstCtrlBlock)
    val fin_E = Output(new InstCtrlBlock)

    val rollback = Output(Bool())
 //   val read_inst_A = Output(new InstCtrlBlock)
  //  val read_inst_B = Output(new InstCtrlBlock)
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

  io.fetch_inst_A := InstFetch.io.IFIDA
  io.fetch_inst_B := InstFetch.io.IFIDB
  io.dispatch_inst_A := Dispatch.io.out_A
  io.dispatch_inst_B := Dispatch.io.out_B
  io.dispatch_inst_C := Dispatch.io.out_C
  io.map_inst_A := RegMap.io.RMDPA
  io.map_inst_B := RegMap.io.RMDPB
  io.com_inst_B := Commit.io.CmtB
  io.com_inst_A := Commit.io.CmtA
  io.rollback := Commit.io.Rollback
  io.com_inst_A :=Commit.io.CmtA
  io.com_inst_B :=Commit.io.CmtB
  io.decode_inst_A :=InstDecode.io.IDRMA
  io.decode_inst_B :=InstDecode.io.IDRMB
  io.read_inst_A := RegRead.io.RREXA
  io.read_inst_B := RegRead.io.RREXB
  io.fin_A := RegRead.io.FinA
  io.fin_B := RegRead.io.FinB
 // io.exe_inst_A := Execute.io.EXMEM.inst
  io.fin_C := Execute.io.FinC
  io.fin_D := Execute.io.FinD
  io.fin_E := Memory.io.FinE

  io.pc := InstFetch.io.addressout
  InstFetch.io.Inst_In_A := io.Inst_A
  InstFetch.io.Inst_In_B := io.Inst_B


  // pipeline
  InstFetch.io.IFIDA <> InstDecode.io.IFIDA
  InstFetch.io.IFIDB <> InstDecode.io.IFIDB
  InstDecode.io.IDRMA <> RegMap.io.IDRMA //RegMap的输入改名了
  InstDecode.io.IDRMB <> RegMap.io.IDRMB
  RegMap.io.RMDPA <> Dispatch.io.in_A//RegMap的输出改名了
  RegMap.io.RMDPB <> Dispatch.io.in_B //dispatch的输入改名了
  Dispatch.io.out_A <> RegRead.io.DPRRA //dispatch的输出改名了
  Dispatch.io.out_B <> RegRead.io.DPRRB
  Dispatch.io.out_C  <> RegRead.io.DPRRC
  RegRead.io.RREXA <> Execute.io.RREXA
  RegRead.io.RREXB <> Execute.io.RREXB
  RegRead.io.RREXC <> Execute.io.RREXC
  Execute.io.EXMEM <> Memory.io.EXMEM



  // ReOrder
  Commit.io.EnA <> RegMap.io.RMDPA
  Commit.io.EnB <> RegMap.io.RMDPB
  Commit.io.ReOrderNumA <> RegMap.io.ReOrderNumA //ReorderNumA改成了num_A
  Commit.io.ReOrderNumB <> RegMap.io.ReOrderNumB
  Commit.io.FinA <> RegRead.io.FinA
  Commit.io.FinB <> RegRead.io.FinB
  Commit.io.FinC <> Execute.io.FinC
  Commit.io.FinD <> Execute.io.FinD
  Commit.io.FinE <> Memory.io.FinE
  Commit.io.CmtA <> InstFetch.io.CmtA
  Commit.io.CmtA <> RegMap.io.CmtA//CmtA改成了cmt_A
  Commit.io.CmtB <> RegMap.io.CmtB
  Commit.io.CmtA <> Memory.io.CmtA

  // dispatch
  Dispatch.io.regstate <> RegMap.io.PhyRegStates //PhyRegStates改成了regstate

  // map execute
  RegMap.io.FinA <> RegRead.io.FinA //RegMap中FinA改成了fin_A
  RegMap.io.FinB <> RegRead.io.FinB
  RegMap.io.FinC <> Execute.io.FinC
  RegMap.io.FinD <> Execute.io.FinD
  RegMap.io.FinE <> Memory.io.FinE
  RegRead.io.FinC <> Execute.io.FinC
  RegRead.io.FinD <> Execute.io.FinD
  RegRead.io.FinE <> Memory.io.FinE

  // block
  val FetchBlock = (Dispatch.io.fetchblock) || Commit.io.FetchBlock
  val enable = !FetchBlock
  io.fetchBlock := Dispatch.io.fetchblock//test
  FetchBlock <> InstFetch.io.FetchBlock
  FetchBlock <> InstDecode.io.FetchBlock
  FetchBlock <> RegMap.io.FetchBlock//RegMap和Dispatch的FetchBlock改成了enable

  // rollback
  Commit.io.Rollback <> InstFetch.io.Rollback
  Commit.io.Rollback <> InstDecode.io.Rollback
  Commit.io.Rollback <> RegMap.io.Rollback //Rollback改成了rollback
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

    val fetch_instA =Output(UInt(32.W))
    val fetch_instB =Output(UInt(32.W))
    val regMap_instA = Output(UInt(32.W))
    val regMap_instB = Output(UInt(32.W))
    val dis_instA = Output(UInt(32.W))
    val dis_instB = Output(UInt(32.W))
    val dis_instC = Output(UInt(32.W))
    val fetchblock = Output(Bool())
    val com_jumppcA = Output(UInt(64.W))
    val com_jumppcB = Output(UInt(64.W))
    val com_bpPredTargetA = Output(UInt(64.W))
    val com_bpPredTargetB = Output(UInt(64.W))
    val regMap_reg1 =  Output(UInt(5.W))
    val regMap_reg2 =  Output(UInt(5.W))
    val regMap_reg3 =  Output(UInt(5.W))
    val regMap_reg4 =  Output(UInt(5.W))
    val regMap_pre1 =  Output(UInt(7.W))
    val regMap_pre3 =  Output(UInt(7.W))
    val regMap_pre2 =  Output(UInt(7.W))
    val regMap_pre4 =  Output(UInt(7.W))
    val regMap_regdesA = Output(UInt(5.W))
    val regMap_regdesB = Output(UInt(5.W))
    val regMap_cmtdesA = Output(UInt(7.W))
    val regMap_cmtdesB = Output(UInt(7.W))
    val regMap_pregdesA = Output(UInt(7.W))
    val regMap_pregdesB = Output(UInt(7.W))
    val decode_reg1 =  Output(UInt(5.W))
    val decode_reg2 =  Output(UInt(5.W))
    val decode_reg3 =  Output(UInt(5.W))
    val decode_reg4 =  Output(UInt(5.W))

    val com_pc =Output(UInt(64.W))
    val src1 = Output(UInt(32.W))
    val src2 = Output(UInt(32.W))
    val src3 = Output(UInt(32.W))
    val src4 = Output(UInt(32.W))
    val com_presrc1 = Output(UInt(7.W))
    val com_presrc2 = Output(UInt(7.W))
    val com_presrc3 = Output(UInt(7.W))
    val com_presrc4 = Output(UInt(7.W))
    val com_cmtdesA = Output(UInt(7.W))
    val com_cmtdesB = Output(UInt(7.W))
    val com_predesA = Output(UInt(7.W))
    val com_predesB = Output(UInt(7.W))
    val com_dataA = Output(UInt(32.W))
    val com_dataB = Output(UInt(32.W))
    val com_instA = Output(UInt(32.W))
    val com_instB = Output(UInt(32.W))
    val read_instA = Output(UInt(32.W))
    val read_instB = Output(UInt(32.W))

    val fin_A_inst = Output(UInt(64.W))
    val fin_B_inst = Output(UInt(64.W))
    val fin_C_inst = Output(UInt(64.W))
    val fin_D_inst = Output(UInt(64.W))
    val fin_E_inst = Output(UInt(64.W))

    val fin_A_jumptarget = Output(UInt(64.W))
    val fin_A_branchtarget = Output(UInt(64.W))
    val fin_B_jumptarget = Output(UInt(64.W))
    val fin_B_branchtarget = Output(UInt(64.W))

    val fin_A_wbdata = Output(UInt(32.W))
    val fin_B_wbdata = Output(UInt(32.W))
    val fin_C_wbdata = Output(UInt(32.W))
    val fin_D_wbdata = Output(UInt(32.W))
    val fin_E_wbdata = Output(UInt(32.W))

    val finA_pregdes = Output(UInt(7.W))
    val finB_pregdes = Output(UInt(7.W))
    val finC_pregdes = Output(UInt(7.W))
    val finD_pregdes = Output(UInt(7.W))
    val finE_pregdes = Output(UInt(7.W))

    val fin_C_finish = Output(Bool())
    val fin_C_pregdes = Output(UInt(32.W))
    val com_rollback =Output(Bool())
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

  data_memory.io.ex_mem.dataAddr := cpu.io.DataRam.data_address
  data_memory.io.ex_mem.writeData := cpu.io.DataRam.data_wdata
  cpu.io.DataRam.data_rdata := data_memory.io.mem_lsu.data
  data_memory.io.ex_mem.func3 := cpu.io.DataRam.func3
  data_memory.io.reset := reset

  // 连接调试信号到模块输出
  io.pc := cpu.io.pc
  io.instA := data_memory.io.mem_id.inst(0)
  io.instB := data_memory.io.mem_id.inst(1)
  io.fetch_instA := cpu.io.fetch_inst_A.inst
  io.fetch_instB := cpu.io.fetch_inst_B.inst

  io.decode_reg1 :=  cpu.io.decode_inst_A.regsrc1
  io.decode_reg2 :=  cpu.io.decode_inst_A.regsrc2
  io.decode_reg3 :=  cpu.io.decode_inst_B.regsrc1
  io.decode_reg4 :=  cpu.io.decode_inst_B.regsrc2

  io.regMap_instB := cpu.io.map_inst_B.inst
  io.regMap_instA := cpu.io.map_inst_A.inst
  io.regMap_reg1 := cpu.io.map_inst_A.regsrc1
  io.regMap_reg2 := cpu.io.map_inst_A.regsrc2
  io.regMap_reg3 := cpu.io.map_inst_B.regsrc1
  io.regMap_reg4 := cpu.io.map_inst_B.regsrc2
  io.regMap_pre1 := cpu.io.map_inst_A.pregsrc1
  io.regMap_pre2 := cpu.io.map_inst_A.pregsrc2
  io.regMap_pre3 := cpu.io.map_inst_B.pregsrc1
  io.regMap_pre4 := cpu.io.map_inst_B.pregsrc1
  io.regMap_regdesA := cpu.io.map_inst_A.regdes
  io.regMap_regdesB := cpu.io.map_inst_B.regdes
  io.regMap_pregdesA := cpu.io.map_inst_A.pregdes
  io.regMap_pregdesB := cpu.io.map_inst_B.pregdes
  io.regMap_cmtdesA := cpu.io.map_inst_A.cmtdes
  io.regMap_cmtdesB := cpu.io.map_inst_B.cmtdes

  io.dis_instA := cpu.io.dispatch_inst_A.inst
  io.dis_instB := cpu.io.dispatch_inst_B.inst
  io.dis_instC := cpu.io.dispatch_inst_C.inst

  io.com_pc :=cpu.io.com_inst_A.pc
  io.src1 := cpu.io.com_inst_A.src1
  io.src2 := cpu.io.com_inst_A.src2
  io.src3 := cpu.io.com_inst_B.src1
  io.src4 := cpu.io.com_inst_B.src2
  io.com_presrc1 := cpu.io.com_inst_A.pregsrc1
  io.com_presrc2 := cpu.io.com_inst_A.pregsrc2
  io.com_presrc3 := cpu.io.com_inst_B.pregsrc1
  io.com_presrc4 := cpu.io.com_inst_B.pregsrc2
  io.com_cmtdesA := cpu.io.com_inst_A.cmtdes
  io.com_cmtdesB := cpu.io.com_inst_B.cmtdes
  io.com_predesA := cpu.io.com_inst_A.pregdes
  io.com_predesB := cpu.io.com_inst_B.pregdes
  io.com_instA := cpu.io.com_inst_A.inst
  io.com_instB := cpu.io.com_inst_B.inst

  io.com_dataA :=cpu.io.com_inst_A.wbdata
  io.com_dataB :=cpu.io.com_inst_B.wbdata
  io.read_instA := cpu.io.read_inst_A.inst
  io.read_instB := cpu.io.read_inst_B.inst

  io.fin_A_inst := cpu.io.fin_A.inst
  io.fin_B_inst := cpu.io.fin_B.inst
  io.fin_C_inst := cpu.io.fin_C.inst
  io.fin_D_inst := cpu.io.fin_D.inst
  io.fin_E_inst := cpu.io.fin_E.inst

  io.fin_A_wbdata := cpu.io.fin_A.wbdata
  io.fin_B_wbdata := cpu.io.fin_B.wbdata
  io.fin_C_wbdata := cpu.io.fin_C.wbdata
  io.fin_D_wbdata := cpu.io.fin_D.wbdata
  io.fin_E_wbdata := cpu.io.fin_E.wbdata

  io.finA_pregdes := cpu.io.fin_A.pregdes
  io.finB_pregdes := cpu.io.fin_B.pregdes
  io.finC_pregdes := cpu.io.fin_C.pregdes
  io.finD_pregdes := cpu.io.fin_D.pregdes
  io.finE_pregdes := cpu.io.fin_E.pregdes

  io.fin_A_jumptarget := cpu.io.fin_A.jump.actTarget
  io.fin_A_branchtarget := cpu.io.fin_A.branch.target
  io.fin_B_jumptarget := cpu.io.fin_B.jump.actTarget
  io.fin_B_branchtarget := cpu.io.fin_B.branch.target
  io.fin_C_pregdes := cpu.io.fin_C.pregdes

  io.fin_C_finish := cpu.io.fin_C.finish
  io.fetchblock := cpu.io.fetchBlock
  io.com_jumppcA := cpu.io.com_inst_A.jump.actTarget
  io.com_jumppcB := cpu.io.com_inst_B.jump.actTarget
  io.com_bpPredTargetA := cpu.io.com_inst_A.bpPredTarget
  io.com_bpPredTargetB := cpu.io.com_inst_B.bpPredTarget
  io.com_rollback := cpu.io.rollback
}

object ExVerilog extends App {
  (new ChiselStage).emitVerilog(new TopWithMemory(), Array("--target-dir", "generated"))
}