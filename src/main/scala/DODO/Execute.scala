package DODO
import chisel3._
import chisel3.util._
//ljlsyfdygv
class Execute extends Module{
  val io = IO(new Bundle{
    val RREXA = Input(new InstCtrlBlock)
    val RREXB = Input(new InstCtrlBlock)
    val RREXC = Input(new InstCtrlBlock)
    val EXMEM = Output(new InstCtrlBlock)

    val FinC = Output(new InstCtrlBlock)
    val FinD = Output(new InstCtrlBlock)

    val Rollback = Input(Bool())
  })

  val INSTA = RegNext(io.RREXA)
  val INSTB = RegNext(io.RREXB)
  val INSTC = RegNext(io.RREXC)

  val ALU1 = Module(new ArithmeticLogicalUnit)
  ALU1.io.isa <> INSTA.isa
  ALU1.io.pc <> INSTA.pc
  ALU1.io.src1 <> INSTA.src1
  ALU1.io.src2 <> INSTA.src2
  ALU1.io.imm <> INSTA.imm
  ALU1.io.csr_rdata <> INSTA.wbdata // 来自CSR的读数据

  val ALU2 = Module(new ArithmeticLogicalUnit)
  ALU2.io.isa <> INSTB.isa
  ALU2.io.pc <> INSTB.pc
  ALU2.io.src1 <> INSTB.src1
  ALU2.io.src2 <> INSTB.src2
  ALU2.io.imm <> INSTB.imm
  ALU2.io.csr_rdata <> INSTB.wbdata // 来自CSR的读数据


  val agu = Module(new AddressGenerationUnit)
  agu.io.isa <> INSTC.isa
  agu.io.src1 <> INSTC.src1
  agu.io.src2 <> INSTC.src2
  agu.io.imm <> INSTC.imm

  when(io.Rollback){
    io.FinC := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.FinD := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.EXMEM := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    io.FinC := GenFin(INSTA.isa.Aclass, ALU1.io.result, INSTA)
    io.FinD := GenFin(INSTB.isa.Aclass, ALU2.io.result, INSTB)
    io.EXMEM := GenICB(agu.io.load, agu.io.store, INSTC)
  }

  def GenICB(load: LoadIssue, store: StoreIssue, RREX: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB.Valid := RREX.Valid
    ICB.inst := RREX.inst
    ICB.pc := RREX.pc
    ICB.isa := RREX.isa
    ICB.finish := RREX.finish
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
    ICB.wbdata := RREX.wbdata
    ICB.jump := RREX.jump
    ICB.branch := RREX.branch
    ICB.load := load
    ICB.store := store
    ICB.csr_addr := RREX.csr_addr
    ICB.csr_wdata := RREX.csr_wdata
    ICB.bpPredTaken := RREX.bpPredTaken
    ICB.bpPredTarget := RREX.bpPredTarget
    ICB.bppredIndex := RREX.bppredIndex
    ICB
  }

  def GenFin(finish: Bool, wbdata: UInt, RREX: InstCtrlBlock): InstCtrlBlock = {
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
    ICB.wbdata := wbdata
    ICB.jump := RREX.jump
    ICB.branch := RREX.branch
    ICB.load := RREX.load
    ICB.store := RREX.store
    ICB.csr_addr := RREX.csr_addr
    ICB.csr_wdata := RREX.csr_wdata
    ICB.bpPredTaken := RREX.bpPredTaken
    ICB.bpPredTarget := RREX.bpPredTarget
    ICB.bppredIndex := RREX.bppredIndex
    ICB
  }

}

class ArithmeticLogicalUnit extends Module {
  val io = IO(new Bundle {
    val isa = Input(new ISA)
    val pc = Input(UInt(32.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val imm = Input(new IMM)
    val result = Output(UInt(32.W))
    val csr_rdata = Input(UInt(32.W)) // 来自CSR的读数据
  })

  //Arithmetic
  val addi = io.isa.ADDI & (io.src1 + io.imm.I)
  val add  = io.isa.ADD  & (io.src1 + io.src2)
  val lui  = io.isa.LUI  & io.imm.U
  val sub  = io.isa.SUB  & (io.src1 - io.src2)
  val Arithmetic = addi | add | lui | sub
  //Logical
  val andi = io.isa.ANDI & (io.src1 & io.imm.I)
  val and  = io.isa.AND  & (io.src1 & io.src2)
  val ori  = io.isa.ORI  & (io.src1 | io.imm.I)
  val or   = io.isa.OR   & (io.src1 | io.src2)
  val xori = io.isa.XORI & (io.src1 ^ io.imm.I)
  val xor  = io.isa.XOR  & (io.src1 ^ io.src2)
  val Logical = andi | and | ori | or | xori | xor

  //Compare
  val slt   = io.isa.SLT   & Mux(io.src1.asSInt < io.src2.asSInt, 1.U, 0.U)
  val slti  = io.isa.SLTI  & Mux(io.src1.asSInt < io.imm.I.asSInt, 1.U, 0.U)
  val sltu  = io.isa.SLTU  & Mux(io.src1 < io.src2, 1.U, 0.U)
  val sltiu = io.isa.SLTIU & Mux(io.src1 < io.imm.I, 1.U, 0.U)
  val Compare = slt | slti | sltu | sltiu
  //Shifts
  private def getShiftAmount(useImm: Bool) =
    Mux(useImm, io.imm.I(4,0), io.src2(4,0))
  val shiftReg = getShiftAmount(false.B)
  val sll  = io.isa.SLL  & (io.src1 << shiftReg)(31,0)
  val srl  = io.isa.SRL  & (io.src1 >> shiftReg)
  val sra  = io.isa.SRA  & (io.src1.asSInt >> shiftReg).asUInt
  // 立即数移位版本（使用imm.I作为移位量）
  val shiftImm = getShiftAmount(true.B)
  val slli = io.isa.SLLI & (io.src1 << shiftImm)(31,0)
  val srli = io.isa.SRLI & (io.src1 >> shiftImm)
  val srai = io.isa.SRAI & (io.src1.asSInt >> shiftImm).asUInt

  val Shifts = sll | srl | sra | slli | srli | srai

  val link = SignExt((io.isa.JAL | io.isa.JALR).asUInt, 32) & (io.pc + 4.U)
  val auipc = SignExt(io.isa.AUIPC.asUInt, 32) & (io.pc + io.imm.U)

  val csr_result = Mux(io.isa.CSRRW, io.csr_rdata, 0.U) //csr
  io.result := Arithmetic | Logical | Compare | Shifts | link | auipc | csr_result
}

class AddressGenerationUnit extends Module{
  val io = IO(new Bundle{
    val isa = Input(new ISA)
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val imm = Input(new IMM)
    val load = Output(new LoadIssue)
    val store = Output(new StoreIssue)
  })

  io.load.Valid := io.isa.Lclass
  io.load.addr := io.src1 + io.imm.I
  io.load.data := 0.U
  io.load.Ready := false.B

  io.store.Valid := io.isa.Sclass
  io.store.addr := io.src1 + io.imm.S


  val SW_data = io.isa.SW & io.src2(31,0)
  val SH_data = io.isa.SH & Cat(Fill(16, io.src2(15)), io.src2(15,0))
  val SB_data = io.isa.SB & Cat(Fill(24, io.src2(7)), io.src2(7,0))
  io.store.data := SW_data | SH_data | SB_data
  val byte_mask = Wire(UInt(3.W)) // 3位掩码编码
  when(io.isa.SW) {
    byte_mask := 2.U  // SW指令编码为2
  }.elsewhen(io.isa.SH) {
    byte_mask := 1.U  // SH指令编码为1
  }.otherwise {       // SB指令
    byte_mask := 0.U  // SB指令编码为0
  }
  io.store.mask := byte_mask
  io.store.Ready := false.B
}//AGU
