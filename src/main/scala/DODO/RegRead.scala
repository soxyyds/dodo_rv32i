package DODO

import chisel3._
import chisel3.util._
import DODO.RegMap.AbstractRegBank
class RegRead extends Module{
  val io = IO(new Bundle{
    val DPRRA = Input(new InstCtrlBlock)
    val DPRRB = Input(new InstCtrlBlock)
    val DPRRC = Input(new InstCtrlBlock)
    val RREXA = Output(new InstCtrlBlock)
    val RREXB = Output(new InstCtrlBlock)
    val RREXC = Output(new InstCtrlBlock)

    val FinA = Output(new InstCtrlBlock)
    val FinB = Output(new InstCtrlBlock)
    val FinC = Input(new InstCtrlBlock)
    val FinD = Input(new InstCtrlBlock)
    val FinE = Input(new InstCtrlBlock)

    val Rollback = Input(Bool())
  })

  val INSTA = RegNext(io.DPRRA)
  val INSTB = RegNext(io.DPRRB)
  val INSTC = RegNext(io.DPRRC)

  val PhyRegFile = new AbstractRegBank(128, 64)
  val src1 = PhyRegFile.read(INSTA.pregsrc1)
  val src2 = PhyRegFile.read(INSTA.pregsrc2)
  val src3 = PhyRegFile.read(INSTB.pregsrc1)
  val src4 = PhyRegFile.read(INSTB.pregsrc2)
  val src5 = PhyRegFile.read(INSTC.pregsrc1)
  val src6 = PhyRegFile.read(INSTC.pregsrc2)

  PhyRegFile.write(io.FinA.Valid && io.FinA.finish, io.FinA.pregdes, io.FinA.wbdata)
  PhyRegFile.write(io.FinB.Valid && io.FinB.finish, io.FinB.pregdes, io.FinB.wbdata)
  PhyRegFile.write(io.FinC.Valid && io.FinC.finish, io.FinC.pregdes, io.FinC.wbdata)
  PhyRegFile.write(io.FinD.Valid && io.FinD.finish, io.FinD.pregdes, io.FinD.wbdata)
  PhyRegFile.write(io.FinE.Valid && io.FinE.finish, io.FinE.pregdes, io.FinE.wbdata)

  val BJU1 = Module(new BranchJumpUnit)
  BJU1.io.isa <> INSTA.isa
  BJU1.io.pc <> INSTA.pc
  BJU1.io.src1 <> src1
  BJU1.io.src2 <> src2
  BJU1.io.imm <> INSTA.imm

  val BJU2 = Module(new BranchJumpUnit)
  BJU2.io.isa <> INSTB.isa
  BJU2.io.pc <> INSTB.pc
  BJU2.io.src1 <> src3
  BJU2.io.src2 <> src4
  BJU2.io.imm <> INSTB.imm

  val InstAisHalt = INSTA.inst(6,0) === "h6b".U(7.W)
  val InstBisHalt = INSTB.inst(6,0) === "h6b".U(7.W)
  val InstAisPrint = INSTA.inst(6,0) === "h7b".U(7.W)
  val InstBisPrint = INSTB.inst(6,0) === "h7b".U(7.W)
  val Afinish = INSTA.isa.Bclass || INSTA.isa.Jclass || InstAisHalt || InstAisPrint
  val Bfinish = INSTB.isa.Bclass || INSTB.isa.Jclass || InstBisHalt || InstBisPrint

  when(io.Rollback){
    io.RREXA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.RREXB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.RREXC := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.FinA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.FinB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    io.RREXA := GenICB(src1, src2, INSTA)
    io.RREXB := GenICB(src3, src4, INSTB)
    io.RREXC := GenICB(src5, src6, INSTC)
    io.FinA := GenFin(Afinish, BJU1.io.jump, BJU1.io.branch, INSTA)
    io.FinB := GenFin(Bfinish, BJU2.io.jump, BJU2.io.branch, INSTB)
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
    ICB.csr_addr := decode.io.csr_addr
    ICB
  }

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
    ICB
  }
}

class BranchJumpUnit extends Module{
  val io = IO(new Bundle{
    val isa = Input(new ISA)
    val pc = Input(UInt(64.W))
    val src1 = Input(UInt(64.W))
    val src2 = Input(UInt(64.W))
    val imm = Input(new IMM)

    val branch = Output(new BranchIssue)
    val jump = Output(new JumpIssue)
  })

  val beq = io.isa.BEQ && (io.src1 === io.src2)
  val bne = io.isa.BNE && (io.src1 =/= io.src2)
  val bgeu = io.isa.BGEU && (io.src1 >= io.src2)
  val bltu = io.isa.BLTU && (io.src1 < io.src2)
  val bge = io.isa.BGE && (io.src1.asSInt >= io.src2.asSInt)
  val blt = io.isa.BLT && (io.src1.asSInt < io.src2.asSInt)

  val b_target = io.pc + io.imm.B
  val jal_target = io.pc + io.imm.J
  val jalr_target = io.src1 + io.imm.I

  io.jump.Valid := io.isa.JAL || io.isa.JALR
  io.jump.proTarget := 0.U
  io.jump.actTarget := Mux(io.isa.JAL, jal_target, Mux(io.isa.JALR, jalr_target, 0.U))
  io.jump.link := io.pc + 4.U

  io.branch.Valid := io.isa.BEQ || io.isa.BNE || io.isa.BGEU || io.isa.BLTU || io.isa.BGE || io.isa.BLT
  io.branch.proTaken := false.B
  io.branch.actTaken := beq || bne || bgeu || bltu || bge || blt
  io.branch.target := b_target

}