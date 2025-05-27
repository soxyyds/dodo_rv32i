package DODO

import chisel3._
import chisel3.util._


class RegRead extends Module{
  val io = IO(new Bundle{
    //Dispatch->RegRead
    val DPRRA = Input(new InstCtrlBlock)
    val DPRRB = Input(new InstCtrlBlock)
    val DPRRC = Input(new InstCtrlBlock)
    //Dispatch模块会一次输出三条指令到寄存器读取阶段，故有三个输入和输出
    //其中AB两条是计算类或者分支跳转类，C是寄存器读写
    //RegRead->Execute
    val RREXA = Output(new InstCtrlBlock)
    val RREXB = Output(new InstCtrlBlock)
    val RREXC = Output(new InstCtrlBlock)

    //FinA/B代表如果AB两条指令是分支跳转类的话，指令的完成情况
    //FinC/D同理代表AB两条指令是计算类的话，指令的完成情况
    //FinE代表C指令是寄存器读写类的话，指令的完成情况
    val FinA = Output(new InstCtrlBlock)
    val FinB = Output(new InstCtrlBlock)
    val FinC = Input(new InstCtrlBlock)
    val FinD = Input(new InstCtrlBlock)
    val FinE = Input(new InstCtrlBlock)

    val Rollback = Input(Bool())

  })

  //将指令存入寄存器，形成流水线寄存器
  val INSTA = RegNext(io.DPRRA)
  val INSTB = RegNext(io.DPRRB)
  val INSTC = RegNext(io.DPRRC)

  val PhyRegFile = new AbstractRegBank(128,32)

  val src1 = PhyRegFile.read(INSTA.pregsrc1)
  val src2 = PhyRegFile.read(INSTA.pregsrc2)
  val src3 = PhyRegFile.read(INSTB.pregsrc1)
  val src4 = PhyRegFile.read(INSTB.pregsrc2)
  val src5 = PhyRegFile.read(INSTC.pregsrc1)
  val src6 = PhyRegFile.read(INSTC.pregsrc2)

  // 在 RegRead 模块中，添加中间寄存器


  // 调用 write 方法时，使用中间寄存器,在这里写了
  PhyRegFile.write(io.FinA.Valid && io.FinA.finish, io.FinA.pregdes, io.FinA.wbdata)
  PhyRegFile.write(io.FinB.Valid && io.FinB.finish, io.FinB.pregdes, io.FinB.wbdata)
  PhyRegFile.write(io.FinC.Valid && io.FinC.finish, io.FinC.pregdes, io.FinC.wbdata)
  PhyRegFile.write(io.FinD.Valid && io.FinD.finish, io.FinD.pregdes, io.FinD.wbdata)
  PhyRegFile.write(io.FinE.Valid && io.FinE.finish, io.FinE.pregdes, io.FinE.wbdata)

  val BJU1 = Module(new BranchJumpUnit)//计算跳转的地址
  BJU1.io.isa <> INSTA.isa
  BJU1.io.pc <> INSTA.pc
  BJU1.io.src1 <> src1
  BJU1.io.src2 <> src2
  BJU1.io.imm <> INSTA.imm

  val BJU2 = Module(new BranchJumpUnit)//
  BJU2.io.isa <> INSTB.isa
  BJU2.io.pc <> INSTB.pc
  BJU2.io.src1 <> src3
  BJU2.io.src2 <> src4
  BJU2.io.imm <> INSTB.imm

  //为了便于调试，添加特殊指令halt和print，在这里进行判断
  val InstAisHalt = INSTA.inst(6,0) === "h6b".U(7.W)
  val InstBisHalt = INSTB.inst(6,0) === "h6b".U(7.W)
  val InstAisPrint = INSTA.inst(6,0) === "h7b".U(7.W)
  val InstBisPrint = INSTB.inst(6,0) === "h7b".U(7.W)
  //如果是halt/print指令或分支跳转指令，则在本阶段指令完成，那么commit模块随后会发射指令
  val Afinish = INSTA.isa.Bclass || INSTA.isa.Jclass || InstAisHalt || InstAisPrint
  val Bfinish = INSTB.isa.Bclass || INSTB.isa.Jclass || InstBisHalt || InstBisPrint

  io.RREXA.csr_wdata := 0.U
  io.RREXB.csr_wdata := 0.U
  io.RREXC.csr_wdata := 0.U
  io.FinA.csr_wdata  := 0.U
  io.FinB.csr_wdata  := 0.U
  when(io.Rollback){
    //如果发生回滚，将输出的指令控制块清零
    io.RREXA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.RREXB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.RREXC := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.FinA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.FinB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    //将前一阶段传过来的指令信息继续向下一阶段传递
    io.RREXA := GenICB(src1, src2, INSTA)
    io.RREXB := GenICB(src3, src4, INSTB)
    io.RREXC := GenICB(src5, src6, INSTC)
    //判断两条指令是否为分支跳转类指令，将指令信息赋值给FinA/B
    io.FinA := GenFin(Afinish, BJU1.io.jump, BJU1.io.branch, INSTA)
    io.FinB := GenFin(Bfinish, BJU2.io.jump, BJU2.io.branch, INSTB)
    // csr_addr的传递在上述RREX的赋值中已经包含
    io.RREXA.csr_wdata := src1
    io.RREXB.csr_wdata := src3
  }



  def GenICB(src1: UInt, src2: UInt, DPRR: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB.Valid := DPRR.Valid
    ICB.inst := DPRR.inst
    ICB.pc := DPRR.pc
    ICB.isa := DPRR.isa
    ICB.finish := DPRR.finish
    ICB.reOrderNum := DPRR.reOrderNum
    ICB.regdes := DPRR.regdes
    ICB.regsrc1 := DPRR.regsrc1
    ICB.regsrc2 := DPRR.regsrc2
    ICB.pregsrc1 := DPRR.pregsrc1
    ICB.pregsrc2 := DPRR.pregsrc2
    ICB.pregdes := DPRR.pregdes
    ICB.cmtdes := DPRR.cmtdes
    ICB.src1 := src1
    ICB.src2 := src2
    ICB.imm := DPRR.imm
    ICB.wbdata := DPRR.wbdata
    ICB.jump := DPRR.jump
    ICB.branch := DPRR.branch
    ICB.load := DPRR.load
    ICB.store := DPRR.store
    ICB.csr_addr := DPRR.csr_addr
    ICB.csr_wdata := src1
    ICB.bpPredTaken := DPRR.bpPredTaken
    ICB.bpPredTarget := DPRR.bpPredTarget
    ICB.bppredIndex := DPRR.bppredIndex
    ICB    //返回值，将生成的指令信息返回给调用者
  }

  //在分支跳转类指令完成时，生成包含最新分支跳转信息的指令控制块，供Commit阶段使用
  def GenFin(finish: Bool, jump: JumpIssue, branch: BranchIssue, RREX: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB.Valid := RREX.Valid
    ICB.inst := RREX.inst
    ICB.pc := RREX.pc
    ICB.isa := RREX.isa
    ICB.finish := finish
    ICB.reOrderNum := RREX.reOrderNum
    ICB.regdes := RREX.regdes
    ICB.regsrc1 := RREX.regsrc1
    ICB.regsrc2 := RREX.regsrc2
    ICB.pregsrc1 := RREX.pregsrc1
    ICB.pregsrc2 := RREX.pregsrc2
    ICB.pregdes := RREX.pregdes
    ICB.cmtdes := RREX.cmtdes
    ICB.src1 := RREX.src1
    ICB.src2 := RREX.src2
    ICB.imm := RREX.imm
    ICB.wbdata := jump.link
    ICB.jump := jump
    ICB.branch := branch
    ICB.load := RREX.load
    ICB.store := RREX.store
    ICB.csr_addr := RREX.csr_addr
    ICB.csr_wdata := 0.U
    ICB.bpPredTaken := RREX.bpPredTaken
    ICB.bpPredTarget := RREX.bpPredTarget
    ICB.bppredIndex := RREX.bppredIndex
    ICB
  }
}

//分支跳转类指令的处理模块
class BranchJumpUnit extends Module{
  val io = IO(new Bundle{
    val isa = Input(new ISA)
    val pc = Input(UInt(64.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val imm = Input(new IMM)  //指令的立即数，包含不同类型的立即数

    val branch = Output(new BranchIssue)
    val jump = Output(new JumpIssue)
  })

  //判断是否发生分支，逐个判断
  val beq = io.isa.BEQ && (io.src1 === io.src2)
  val bne = io.isa.BNE && (io.src1 =/= io.src2)
  val bgeu = io.isa.BGEU && (io.src1 >= io.src2)
  val bltu = io.isa.BLTU && (io.src1 < io.src2)
  val bge = io.isa.BGE && (io.src1.asSInt >= io.src2.asSInt)
  val blt = io.isa.BLT && (io.src1.asSInt < io.src2.asSInt)

  //计算 分支目标地址&预测的跳转目标地址
  val b_target = (io.pc + io.imm.B)(31,0)
  val jal_target = (io.pc + io.imm.J)(31,0)
  val jalr_target = ((io.src1 + io.imm.I) & "hfffffffffffffffe".U(64.W))(31,0) //确保地址对齐

  //获取并综合跳转信息
  io.jump.Valid := io.isa.JAL || io.isa.JALR
  io.jump.actTarget := Mux(io.isa.JAL, jal_target, Mux(io.isa.JALR, jalr_target, 0.U))
  io.jump.link := io.pc + 4.U

  //获取并综合分支信息
  io.branch.Valid := io.isa.BEQ || io.isa.BNE || io.isa.BGEU || io.isa.BLTU || io.isa.BGE || io.isa.BLT
  io.branch.actTaken := beq || bne || bgeu || bltu || bge || blt
  io.branch.target := b_target//

}