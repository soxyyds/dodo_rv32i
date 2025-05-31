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
    val d_data = Output(UInt(64.W)) // 数据总线输出
    val mem_inst = Output(new InstCtrlBlock) // 内存指令提交
    val mem_Valid = Output(Bool()) // 内存指令的有效性
    //test
    val fetch_inst_A = Output(new InstCtrlBlock)
    val fetch_inst_B = Output(new InstCtrlBlock)
//    val map_inst_A = Output(new InstCtrlBlock)
//    val map_inst_B = Output(new InstCtrlBlock)
//    val dispatch_inst_A = Output(new InstCtrlBlock)
//    val dispatch_inst_B = Output(new InstCtrlBlock)
//    val dispatch_inst_C = Output(new InstCtrlBlock)
    val fetchBlock = Output (Bool())
//    val decode_inst_A = Output(new InstCtrlBlock)
//    val decode_inst_B = Output(new InstCtrlBlock)
//    val read_inst_A = Output(new InstCtrlBlock)
//    val read_inst_B = Output(new InstCtrlBlock)
 //   val exe_inst_A = Output(new InstCtrlBlock)
 //   val exe_inst_B = Output(new InstCtrlBlock)
 //   val memory_inst_A = Output(new InstCtrlBlock)
 //   val memory_inst_B = Output(new InstCtrlBlock)
 //   val forwardStore = Output(new StoreIssue) // Store Forwarding 输入
 //   val read_func3 = Output(UInt(3.W)) // 读取功能码
    val com_inst_A = Output(new InstCtrlBlock)
    val com_inst_B = Output(new InstCtrlBlock)
    val com_EnQueuePointer = Output(UInt(6.W)) // 提交阶段的队列指针
    val com_DeQueuePointer = Output(UInt(6.W)) // 提交阶段的寄存器指针
 //   val com_bank = Output(Vec(64, new InstCtrlBlock()))
 //   val fin_A = Output(new InstCtrlBlock)
 //   val fin_B = Output(new InstCtrlBlock)
 //   val fin_C = Output(new InstCtrlBlock)
 //   val fin_D = Output(new InstCtrlBlock)
 //   val fin_E = Output(new InstCtrlBlock)

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
//  io.dispatch_inst_A := Dispatch.io.out_A
//  io.dispatch_inst_B := Dispatch.io.out_B
//  io.dispatch_inst_C := Dispatch.io.out_C
//  io.map_inst_A := RegMap.io.RMDPA
//  io.map_inst_B := RegMap.io.RMDPB
  io.com_inst_B := Commit.io.CmtB
  io.com_inst_A := Commit.io.CmtA
  io.rollback := Commit.io.Rollback
  io.com_inst_A :=Commit.io.CmtA
  io.com_inst_B :=Commit.io.CmtB
//  io.decode_inst_A :=InstDecode.io.IDRMA
//  io.decode_inst_B :=InstDecode.io.IDRMB
//  io.read_inst_A := RegRead.io.RREXA
//  io.read_inst_B := RegRead.io.RREXB
 // io.fin_A := RegRead.io.FinA
 // io.fin_B := RegRead.io.FinB
 // io.exe_inst_A := Execute.io.EXMEM.inst
 // io.fin_C := Execute.io.FinC
 // io.fin_D := Execute.io.FinD
 // io.fin_E := Memory.io.FinE
  io.com_DeQueuePointer := Commit.io.DeQueuePointer // 提交阶段的寄存器指针
  io.com_EnQueuePointer := Commit.io.EnQueuePointer // 提交阶段的队列指针
//  io.com_bank := Commit.io.Bank
  io.mem_inst := Memory.io.mem_inst
  io.mem_Valid := Memory.io.mem_Valid
 // io.forwardStore := Commit.io.ForwardStore // Store Forwarding 输出
 // io.read_func3 := Memory.io.read_func3// 读取功能码
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
  Commit.io.CmtB <> RegRead.io.CmtB //CmtB改成了cmt_B
  Commit.io.CmtA <> RegRead.io.CmtA

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
  io.fetchBlock := FetchBlock//test
  FetchBlock <> InstFetch.io.FetchBlock
  FetchBlock <> InstDecode.io.FetchBlock
  Dispatch.io.fetchblock <> RegMap.io.FetchBlock//RegMap和Dispatch的FetchBlock改成了enable

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
  io.d_data := Memory.io.d_data // 数据总线输出

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
//    val regMap_instA = Output(UInt(32.W))
//    val regMap_instB = Output(UInt(32.W))
  //  val dis_instA = Output(UInt(32.W))
 //   val dis_instB = Output(UInt(32.W))
//    val dis_instC = Output(UInt(32.W))
    val fetchblock = Output(Bool())
    val com_jumptakenA = Output(Bool())
    val com_jumptakenB = Output(Bool())
    val com_jumppcA = Output(UInt(64.W))
    val com_jumppcB = Output(UInt(64.W))
    val com_branchtargetA = Output(UInt(64.W))
    val com_branchtargetB = Output(UInt(64.W))
    val com_bpPredTargetA = Output(UInt(64.W))
    val com_bpPredTargetB = Output(UInt(64.W))
//    val regMap_reg1 =  Output(UInt(5.W))
 //   val regMap_reg2 =  Output(UInt(5.W))
 //   val regMap_reg3 =  Output(UInt(5.W))
 //   val regMap_reg4 =  Output(UInt(5.W))
  //  val regMap_pre1 =  Output(UInt(7.W))
 //   val regMap_pre3 =  Output(UInt(7.W))
 //   val regMap_pre2 =  Output(UInt(7.W))
//    val regMap_pre4 =  Output(UInt(7.W))
//    val regMap_regdesA = Output(UInt(5.W))
//    val regMap_regdesB = Output(UInt(5.W))
//    val regMap_cmtdesA = Output(UInt(7.W))
//    val regMap_cmtdesB = Output(UInt(7.W))
//    val regMap_pregdesA = Output(UInt(7.W))
 //   val regMap_pregdesB = Output(UInt(7.W))
//    val decode_reg1 =  Output(UInt(5.W))
 //   val decode_reg2 =  Output(UInt(5.W))
//    val decode_reg3 =  Output(UInt(5.W))
 //   val decode_reg4 =  Output(UInt(5.W))

    val com_pc =Output(UInt(64.W))
 //   val src1 = Output(UInt(32.W))
 //   val src2 = Output(UInt(32.W))
 //   val src3 = Output(UInt(32.W))
 //   val src4 = Output(UInt(32.W))
    val com_EnQueuePointer = Output(UInt(6.W)) // 提交阶段的队列指针
    val com_DeQueuePointer = Output(UInt(6.W)) // 提交阶段的寄存器指针
//    val com_reorderNumA = Output(UInt(6.W))
//    val com_reorderNumB = Output(UInt(6.W))
    val com_bpPredTakenA = Output(Bool())
    val com_bpPredTakenB = Output(Bool())
    val com_branchtakenA = Output(Bool())
    val com_branchtakenB = Output(Bool())
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
//    val read_instA = Output(UInt(32.W))
//    val read_instB = Output(UInt(32.W))
//
//    val fin_A_inst = Output(UInt(64.W))
//    val fin_B_inst = Output(UInt(64.W))
//    val fin_C_inst = Output(UInt(64.W))
//    val fin_D_inst = Output(UInt(64.W))
//    val fin_E_inst = Output(UInt(64.W))
//
//    val fin_A_jumptarget = Output(UInt(64.W))
//    val fin_A_branchtarget = Output(UInt(64.W))
//    val fin_B_jumptarget = Output(UInt(64.W))
//    val fin_B_branchtarget = Output(UInt(64.W))
//
//    val fin_A_wbdata = Output(UInt(32.W))
//    val fin_B_wbdata = Output(UInt(32.W))
//    val fin_C_wbdata = Output(UInt(32.W))
//    val fin_D_wbdata = Output(UInt(32.W))
//    val fin_E_wbdata = Output(UInt(32.W))
//
//    val finA_pregdes = Output(UInt(7.W))
//    val finB_pregdes = Output(UInt(7.W))
//    val finC_pregdes = Output(UInt(7.W))
//    val finD_pregdes = Output(UInt(7.W))
//    val finE_pregdes = Output(UInt(7.W))
//
//    val fin_C_finish = Output(Bool())
//    val fin_C_pregdes = Output(UInt(32.W))
    val com_rollback =Output(Bool())
    val mem_writeEnable = Output(Bool()) // 暴露内存写使能信号
    val mem_writeAddr = Output(UInt(64.W)) // 暴露内存写地址
    val mem_readAddr = Output(UInt(64.W)) // 暴露内存读地址
    val mem_writeData = Output(UInt(32.W)) // 暴露内存写数据
    val mem_func3_write = Output(UInt(3.W)) // 暴露内存读地址
    val mem_func3_read = Output(UInt(3.W)) // 暴露内存读功能码
    val mem_rdata = Output(UInt(32.W)) // 暴露内存读数据
    val mem_inst = Output(UInt(32.W)) // 暴露内存指令提交
    val mem_Valid = Output(Bool()) // 暴露内存指令的有效性
    val mem_fwdata = Output(UInt(64.W)) // 暴露内存转发数据
  //  val com_bank = Output(Vec(64, new InstCtrlBlock()))
  //  val read_func3 = Output(UInt(3.W)) // 暴露读取功能码
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
  io.fetch_instA := cpu.io.fetch_inst_A.inst
  io.fetch_instB := cpu.io.fetch_inst_B.inst

  //io.decode_reg1 :=  cpu.io.decode_inst_A.regsrc1
 // io.decode_reg2 :=  cpu.io.decode_inst_A.regsrc2
 // io.decode_reg3 :=  cpu.io.decode_inst_B.regsrc1
  //io.decode_reg4 :=  cpu.io.decode_inst_B.regsrc2

//  io.regMap_instB := cpu.io.map_inst_B.inst
//  io.regMap_instA := cpu.io.map_inst_A.inst
 // io.regMap_reg1 := cpu.io.map_inst_A.regsrc1
 // io.regMap_reg2 := cpu.io.map_inst_A.regsrc2
 // io.regMap_reg3 := cpu.io.map_inst_B.regsrc1
 // io.regMap_reg4 := cpu.io.map_inst_B.regsrc2
//  io.regMap_pre1 := cpu.io.map_inst_A.pregsrc1
 // io.regMap_pre2 := cpu.io.map_inst_A.pregsrc2
 // io.regMap_pre3 := cpu.io.map_inst_B.pregsrc1
 // io.regMap_pre4 := cpu.io.map_inst_B.pregsrc1
//  io.regMap_regdesA := cpu.io.map_inst_A.regdes
//  io.regMap_regdesB := cpu.io.map_inst_B.regdes
//  io.regMap_pregdesA := cpu.io.map_inst_A.pregdes
//  io.regMap_pregdesB := cpu.io.map_inst_B.pregdes
//  io.regMap_cmtdesA := cpu.io.map_inst_A.cmtdes
//  io.regMap_cmtdesB := cpu.io.map_inst_B.cmtdes
//
//  io.dis_instA := cpu.io.dispatch_inst_A.inst
//  io.dis_instB := cpu.io.dispatch_inst_B.inst
//  io.dis_instC := cpu.io.dispatch_inst_C.inst

  io.com_pc :=cpu.io.com_inst_A.pc
//  io.src1 := cpu.io.com_inst_A.src1
//  io.src2 := cpu.io.com_inst_A.src2
//  io.src3 := cpu.io.com_inst_B.src1
//  io.src4 := cpu.io.com_inst_B.src2
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
//  io.com_reorderNumA := cpu.io.com_inst_A.reOrderNum
//  io.com_reorderNumB := cpu.io.com_inst_B.reOrderNum
//  io.read_instA := cpu.io.read_inst_A.inst
//  io.read_instB := cpu.io.read_inst_B.inst

//  io.fin_A_inst := cpu.io.fin_A.inst
//  io.fin_B_inst := cpu.io.fin_B.inst
//  io.fin_C_inst := cpu.io.fin_C.inst
//  io.fin_D_inst := cpu.io.fin_D.inst
 // io.fin_E_inst := cpu.io.fin_E.inst

 // io.fin_A_wbdata := cpu.io.fin_A.wbdata
//  io.fin_B_wbdata := cpu.io.fin_B.wbdata
//  io.fin_C_wbdata := cpu.io.fin_C.wbdata
//  io.fin_D_wbdata := cpu.io.fin_D.wbdata
//  io.fin_E_wbdata := cpu.io.fin_E.wbdata

 // io.finA_pregdes := cpu.io.fin_A.pregdes
 // io.finB_pregdes := cpu.io.fin_B.pregdes
 // io.finC_pregdes := cpu.io.fin_C.pregdes
 // io.finD_pregdes := cpu.io.fin_D.pregdes
 // io.finE_pregdes := cpu.io.fin_E.pregdes

 // io.fin_A_jumptarget := cpu.io.fin_A.jump.actTarget
 // io.fin_A_branchtarget := cpu.io.fin_A.branch.target
 // io.fin_B_jumptarget := cpu.io.fin_B.jump.actTarget
 // io.fin_B_branchtarget := cpu.io.fin_B.branch.target
 // io.fin_C_pregdes := cpu.io.fin_C.pregdes

 // io.fin_C_finish := cpu.io.fin_C.finish
  io.fetchblock := cpu.io.fetchBlock

  io.com_bpPredTargetA := cpu.io.com_inst_A.bpPredTarget
  io.com_bpPredTargetB := cpu.io.com_inst_B.bpPredTarget
  io.com_rollback := cpu.io.rollback

  io.com_jumptakenA := cpu.io.com_inst_A.jump.Valid
  io.com_jumptakenB := cpu.io.com_inst_B.jump.Valid
  io.com_jumppcA := cpu.io.com_inst_A.jump.actTarget
  io.com_jumppcB := cpu.io.com_inst_B.jump.actTarget
  io.com_branchtargetA := cpu.io.com_inst_A.branch.target
  io.com_branchtargetB := cpu.io.com_inst_B.branch.target
  io.com_bpPredTakenA := cpu.io.com_inst_A.bpPredTaken
  io.com_bpPredTakenB := cpu.io.com_inst_B.bpPredTaken
  io.com_branchtakenA := cpu.io.com_inst_A.branch.Valid
  io.com_branchtakenB := cpu.io.com_inst_B.branch.Valid

  io.com_EnQueuePointer := cpu.io.com_EnQueuePointer // 提交阶段的队列指针
  io.com_DeQueuePointer := cpu.io.com_DeQueuePointer // 提交阶段的寄存器指针
//  io.com_bank := cpu.io.com_bank // 暴露
   io.mem_writeEnable := cpu.io.DataRam.data_wen // 暴露内存写使能信号
   io.mem_writeAddr := cpu.io.DataRam.data_address // 暴露内存写地址
  io.mem_readAddr := cpu.io.DataRam.read_address // 暴露内存读地址
   io.mem_writeData := cpu.io.DataRam.data_wdata // 暴露内存写数据
  io.mem_rdata := data_memory.io.mem_lsu.data // 暴露内存读数据
  io.mem_func3_write := data_memory.io.ex_mem.func3_write // 暴露内存读地址
  io.mem_func3_read := data_memory.io.ex_mem.func3_read // 暴露内存读功能码
  io.mem_Valid := cpu.io.mem_Valid // 暴露内存指令的有效性
  io.mem_inst := cpu.io.mem_inst.inst // 暴露内存指令提交
  io.mem_fwdata := cpu.io.d_data
 // io.read_func3 := cpu.io.read_func3 // 暴露读取功能码
}

object ExVerilog extends App {
  (new ChiselStage).emitVerilog(new TopWithMemory(), Array("--target-dir", "generated"))
}