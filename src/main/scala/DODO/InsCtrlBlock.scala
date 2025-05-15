package DODO

import chisel3._

class InstCtrlBlock extends Bundle{
  //base
  val Valid :Bool = Bool()
  val inst: UInt = UInt(32.W)
  val pc: UInt = UInt(32.W)
  val isa = new ISA
  //reorder
  val finish: Bool = Bool()
  val reOrderNum :UInt= UInt(6.W)
  //regs
  val regdes :UInt= UInt(5.W)
  val regsrc1 :UInt= UInt(5.W)
  val regsrc2 :UInt= UInt(5.W)
  //preg
  val pregsrc1 :UInt= UInt(7.W)
  val pregsrc2 :UInt= UInt(7.W)
  val pregdes :UInt= UInt(7.W)
  val cmtdes :UInt= UInt(7.W)
  //exe
  val src1 :UInt= UInt(32.W)
  val src2 :UInt= UInt(32.W)
  val imm :UInt= UInt(32.W)
  val wbdata :UInt= UInt(32.W)
  //bundle
  val jump = new JumpIssue
  val branch = new BranchIssue
  val load = new LoadIssue
  val store = new StoreIssue
  //csrrw
  val csr_addr :UInt= UInt(12.W)  // CSR地址
  val csr_wdata :UInt= UInt(32.W) // 要写入CSR的数据
}
class JumpIssue extends Bundle{
  val Valid: Bool = Bool()
  val proTarget :UInt= UInt(32.W)
  val actTarget :UInt= UInt(32.W)
  val link :UInt= UInt(32.W)
}
class BranchIssue extends Bundle{
  val Valid: Bool = Bool()
  val proTaken :UInt= Bool()
  val actTaken :UInt= Bool()
  val target: UInt = UInt(32.W)
}
class LoadIssue extends Bundle{
  val Valid: Bool = Bool()
  val addr :UInt= UInt(32.W)
  val data :UInt= UInt(32.W)
  val Ready: Bool = Bool()
}
class StoreIssue extends Bundle{
  val Valid: Bool = Bool()
  val addr :UInt= UInt(32.W)
  val mask :UInt= UInt(32.W)
  val data :UInt= UInt(32.W)
  val Ready :Bool= Bool()
}

class IMM extends Bundle {
  val I :UInt= UInt(32.W)
  val B :UInt= UInt(32.W)
  val S :UInt= UInt(32.W)
  val U :UInt= UInt(32.W)
  val J :UInt= UInt(32.W)
  val Z :UInt= UInt(32.W)
}


class ISA extends Bundle{
  // Arithmetic
  val ADD: Bool = Bool()
  val ADDI : Bool= Bool()
  val SUB : Bool= Bool()
  val LUI : Bool= Bool()
  val AUIPC : Bool= Bool()
  // Shift
  val SLL : Bool= Bool()
  val SLLI : Bool= Bool()
  val SRL : Bool= Bool()
  val SRLI : Bool= Bool()
  val SRA : Bool= Bool()
  val SRAI : Bool= Bool()
  // Logic
  val XOR : Bool= Bool()
  val XORI : Bool= Bool()
  val OR : Bool= Bool()
  val ORI : Bool= Bool()
  val AND: Bool = Bool()
  val ANDI : Bool= Bool()
  // Compare
  val SLT : Bool= Bool()
  val SLTU: Bool = Bool()
  val SLTI: Bool = Bool()
  val SLTIU: Bool = Bool()
  // Branch
  val BEQ: Bool = Bool()
  val BNE : Bool= Bool()
  val BLT : Bool= Bool()
  val BGE : Bool= Bool()
  val BLTU : Bool= Bool()
  val BGEU : Bool= Bool()
  // Jump
  val JAL : Bool= Bool()
  val JALR : Bool= Bool()
  // Store
  val SB : Bool= Bool()
  val SH : Bool= Bool()
  val SW : Bool= Bool()
  // Load
  val LB : Bool= Bool()
  val LBU : Bool= Bool()
  val LH : Bool= Bool()
  val LHU : Bool= Bool()
  val LW : Bool= Bool()
  // csrrw
  val CSRRW : Bool= Bool()

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
object InstCtrlBlock {
  def =/=(a: InstCtrlBlock, b: InstCtrlBlock): Bool = {
    a.asUInt =/= b.asUInt  // 整体比较
  }
}