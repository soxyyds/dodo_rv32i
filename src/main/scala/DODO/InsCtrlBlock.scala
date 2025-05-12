package DODO

import chisel3._
import chisel3.util._

class InstCtrlBlock extends Bundle{
  //base
  val Valid = Bool()
  val inst = UInt(32.W)
  val pc = UInt(64.W)
  val isa = new ISA
  //reorder
  val finish = Bool()
  val reOrderNum = UInt(6.W)
  //regs
  val regdes = UInt(5.W)
  val regsrc1 = UInt(5.W)
  val regsrc2 = UInt(5.W)
  //preg
  val pregsrc1 = UInt(7.W)
  val pregsrc2 = UInt(7.W)
  val pregdes = UInt(7.W)
  val cmtdes = UInt(7.W)
  //exe
  val src1 = UInt(32.W)
  val src2 = UInt(32.W)
  val imm = UInt(32.W)
  val wbdata = UInt(32.W)
  //bundle
  val jump = new JumpIssue
  val branch = new BranchIssue
  val load = new LoadIssue
  val store = new StoreIssue
  //
}
class JumpIssue extends Bundle{
  val Valid = Bool()
  val proTarget = UInt(64.W)
  val actTarget = UInt(64.W)
  val link = UInt(64.W)
}
class BranchIssue extends Bundle{
  val Valid = Bool()
  val proTaken = Bool()
  val actTaken = Bool()
  val target = UInt(64.W)
}
class LoadIssue extends Bundle{
  val Valid = Bool()
  val addr = UInt(64.W)
  val data = UInt(32.W)
  val Ready = Bool()
}
class StoreIssue extends Bundle{
  val Valid = Bool()
  val addr = UInt(64.W)
  val mask = UInt(32.W)
  val data = UInt(32.W)
  val Ready = Bool()
}


class ISA extends Bundle{
  // Arithmetic
  val ADD = Bool()
  val ADDI = Bool()
  val SUB = Bool()
  val LUI = Bool()
  val AUIPC = Bool()
  // Shift
  val SLL = Bool()
  val SLLI = Bool()
  val SRL = Bool()
  val SRLI = Bool()
  val SRA = Bool()
  val SRAI = Bool()
  // Logic
  val XOR = Bool()
  val XORI = Bool()
  val OR = Bool()
  val ORI = Bool()
  val AND = Bool()
  val ANDI = Bool()
  // Compare
  val SLT = Bool()
  val SLTU = Bool()
  val SLTI = Bool()
  val SLTIU = Bool()
  // Branch
  val BEQ = Bool()
  val BNE = Bool()
  val BLT = Bool()
  val BGE = Bool()
  val BLTU = Bool()
  val BGEU = Bool()
  // Jump
  val JAL = Bool()
  val JALR = Bool()
  // Store
  val SB = Bool()
  val SH = Bool()
  val SW = Bool()
  // Load
  val LB = Bool()
  val LBU = Bool()
  val LH = Bool()
  val LHU = Bool()
  val LW = Bool()
  // csrrw
  val CSRRW = Bool()

  def Aclass(): Bool = {
    val Arithmetic = ADD || ADDI || SUB || LUI || AUIPC
    val Logical = XOR || XORI || OR || ORI || AND || ANDI
    val Shifts = SLL || SLLI || SRL || SRLI || SRA || SRAI
    val Compare = SLT || SLTI || SLTU || SLTIU
    val ALU = Arithmetic || Logical || Shifts || Compare
    ALU
  }
  def Bclass(): Bool = {
    val Branch = BEQ || BNE || BLT || BGE || BLTU || BGEU
    Branch
  }
  def Jclass(): Bool = {
    val Jump = JAL || JALR
    Jump
  }
  def Sclass(): Bool = {
    val Store = SW || SH || SB
    Store
  }
  def Lclass(): Bool = {
    val Load = LW || LH || LB || LHU || LBU
    Load
  }
}
