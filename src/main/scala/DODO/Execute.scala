package DODO
import chisel3._
import chisel3.util._
import DODO.SignExt

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

  val ALU2 = Module(new ArithmeticLogicalUnit)
  ALU2.io.isa <> INSTB.isa
  ALU2.io.pc <> INSTB.pc
  ALU2.io.src1 <> INSTB.src1
  ALU2.io.src2 <> INSTB.src2
  ALU2.io.imm <> INSTB.imm

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
    ICB
  }

}

class ArithmeticLogicalUnit extends Module{
  val io = IO(new Bundle{
    val isa = Input(new ISA)
    val pc = Input(UInt(64.W))
    val src1 = Input(UInt(64.W))
    val src2 = Input(UInt(64.W))
    val imm = Input(new IMM)
    val result = Output(UInt(64.W))
  })

  //Arithmetic
  val addi	= SignExt(io.isa.ADDI.asUInt, 64)	& (io.src1 + io.imm.I)
  val add 	= SignExt(io.isa.ADD.asUInt, 64)	& (io.src1 + io.src2)
  val lui 	= SignExt(io.isa.LUI.asUInt, 64)	& (io.imm.U)
  val sub		= SignExt(io.isa.SUB.asUInt, 64)	& (io.src1 - io.src2)
  val addiw 	= SignExt(io.isa.ADDIW.asUInt, 64)	& SignExt((io.src1 + io.imm.I)(31,0), 64)
  val addw 	= SignExt(io.isa.ADDW.asUInt, 64)	& SignExt((io.src1 + io.src2)(31,0), 64)
  val subw 	= SignExt(io.isa.SUBW.asUInt, 64)	& SignExt((io.src1 - io.src2)(31,0), 64)
  val Arithmetic = addi | add | lui | sub | addiw | addw | subw
  //Logical
  val andi	= SignExt(io.isa.ANDI.asUInt, 64)	& (io.src1 & io.imm.I)
  val and		= SignExt(io.isa.AND.asUInt, 64)	& (io.src1 & io.src2)
  val ori		= SignExt(io.isa.ORI.asUInt, 64)	& (io.src1 | io.imm.I)
  val or		= SignExt(io.isa.OR.asUInt, 64)		& (io.src1 | io.src2)
  val xori	= SignExt(io.isa.XORI.asUInt, 64)	& (io.src1 ^ io.imm.I)
  val xor		= SignExt(io.isa.XOR.asUInt, 64)	& (io.src1 ^ io.src2)
  val Logical = andi | and | ori | or | xori | xor
  //Compare
  val slt 	= Mux((io.isa.SLT 	&& (io.src1.asSInt < io.src2.asSInt)), 	1.U(64.W), 0.U(64.W))
  val slti 	= Mux((io.isa.SLTI 	&& (io.src1.asSInt < io.imm.I.asSInt)),	1.U(64.W), 0.U(64.W))
  val sltu 	= Mux((io.isa.SLTU 	&& (io.src1.asUInt < io.src2.asUInt)), 	1.U(64.W), 0.U(64.W))
  val sltiu 	= Mux((io.isa.SLTIU	&& (io.src1.asUInt < io.imm.I.asUInt)),	1.U(64.W), 0.U(64.W))
  val Compare = slt | slti | sltu | sltiu
  //Shifts
  val sll		= SignExt(io.isa.SLL.asUInt, 64)	& (io.src1 			<< io.src2(5,0))(63,0)
  val srl		= SignExt(io.isa.SRL.asUInt, 64)	& (io.src1 			>> io.src2(5,0))
  val sra		= SignExt(io.isa.SRA.asUInt, 64)	& (io.src1.asSInt 	>> io.src2(5,0)).asUInt
  val slli	= SignExt(io.isa.SLLI.asUInt, 64)	& (io.src1 			<< io.imm.I(5,0))(63,0)
  val srli	= SignExt(io.isa.SRLI.asUInt, 64)	& (io.src1			>> io.imm.I(5,0))
  val srai	= SignExt(io.isa.SRAI.asUInt, 64)	& (io.src1.asSInt	>> io.imm.I(5,0)).asUInt
  val sllw	= SignExt(io.isa.SLLW.asUInt, 64)	& SignExt((io.src1				<< io.src2(4,0))(31,0)	, 64)
  val srlw	= SignExt(io.isa.SRLW.asUInt, 64)	& SignExt((io.src1(31,0)		>> io.src2(4,0))		, 64)
  val sraw	= SignExt(io.isa.SRAW.asUInt, 64)	& SignExt((io.src1(31,0).asSInt	>> io.src2(4,0)).asUInt	, 64)
  val slliw	= SignExt(io.isa.SLLIW.asUInt, 64)	& SignExt((io.src1 				<< io.imm.I(4,0))(31,0)	, 64)
  val srliw	= SignExt(io.isa.SRLIW.asUInt, 64)	& SignExt((io.src1(31,0)		>> io.imm.I(4,0))		, 64)
  val sraiw	= SignExt(io.isa.SRAIW.asUInt, 64)	& SignExt((io.src1(31,0).asSInt >> io.imm.I(4,0)).asUInt, 64)
  val Shifts 	= sll | srl | sra | slli | srli | srai | sllw | srlw | sraw | slliw | srliw | sraiw

  val link    = SignExt((io.isa.JAL | io.isa.JALR).asUInt, 64)  & (io.pc + 4.U)
  val auipc   = SignExt(io.isa.AUIPC.asUInt, 64) & (io.pc + io.imm.U)

  io.result := Arithmetic | Logical | Compare | Shifts | link | auipc

}

class AddressGenerationUnit extends Module{
  val io = IO(new Bundle{
    val isa = Input(new ISA)
    val src1 = Input(UInt(64.W))
    val src2 = Input(UInt(64.W))
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
  val SD_data = SignExt(io.isa.SD.asUInt, 64) & io.src2
  val SW_data = SignExt(io.isa.SW.asUInt, 64) & Cat(io.src2(31,0), io.src2(31,0))
  val SH_data = SignExt(io.isa.SH.asUInt, 64) & Cat(io.src2(15,0), io.src2(15,0), io.src2(15,0), io.src2(15,0))
  val SB_data = SignExt(io.isa.SB.asUInt, 64) & Cat(io.src2(7,0), io.src2(7,0), io.src2(7,0), io.src2(7,0), io.src2(7,0), io.src2(7,0), io.src2(7,0), io.src2(7,0))
  io.store.data := SD_data | SW_data | SH_data | SB_data
  val d_mask = SignExt(io.isa.SD.asUInt, 8) & "b1111_1111".U(8.W)
  val w_mask = SignExt(io.isa.SW.asUInt, 8) & ("b0000_1111".U(8.W) << io.store.addr(2,0))
  val h_mask = SignExt(io.isa.SH.asUInt, 8) & ("b0000_0011".U(8.W) << io.store.addr(2,0))
  val b_mask = SignExt(io.isa.SB.asUInt, 8) & ("b0000_0001".U(8.W) << io.store.addr(2,0))
  val byte_mask = d_mask | w_mask | h_mask | b_mask
  //8 bit mask -> 64 bit mask
  val byte_mask_0 = Mux(byte_mask(0).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_1 = Mux(byte_mask(1).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_2 = Mux(byte_mask(2).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_3 = Mux(byte_mask(3).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_4 = Mux(byte_mask(4).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_5 = Mux(byte_mask(5).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_6 = Mux(byte_mask(6).asBool(), "hff".U(8.W), 0.U(8.W))
  val byte_mask_7 = Mux(byte_mask(7).asBool(), "hff".U(8.W), 0.U(8.W))
  io.store.mask := Cat(byte_mask_7, byte_mask_6, byte_mask_5, byte_mask_4, byte_mask_3, byte_mask_2, byte_mask_1, byte_mask_0)
  io.store.Ready := false.B

}