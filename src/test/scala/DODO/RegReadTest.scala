package DODO

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RegReadTest extends AnyFlatSpec with ChiselScalatestTester {
  "RegRead" should "work with InstCtrlBlock poke/expect" in {
    test(new RegRead) { c =>
      val instA = (new InstCtrlBlock).Lit(
        _.Valid -> true.B,
        _.inst -> "h00000013".U,
        _.pc -> 0.U,
        _.isa -> (new ISA).Lit(
          _.ADD -> false.B, _.ADDI -> true.B, _.SUB -> false.B, _.LUI -> false.B, _.AUIPC -> false.B,
          _.SLL -> false.B, _.SLLI -> false.B, _.SRL -> false.B, _.SRLI -> false.B, _.SRA -> false.B, _.SRAI -> false.B,
          _.XOR -> false.B, _.XORI -> false.B, _.OR -> false.B, _.ORI -> false.B, _.AND -> false.B, _.ANDI -> false.B,
          _.SLT -> false.B, _.SLTU -> false.B, _.SLTI -> false.B, _.SLTIU -> false.B,
          _.BEQ -> false.B, _.BNE -> false.B, _.BLT -> false.B, _.BGE -> false.B, _.BLTU -> false.B, _.BGEU -> false.B,
          _.JAL -> false.B, _.JALR -> false.B,
          _.SB -> false.B, _.SH -> false.B, _.SW -> false.B,
          _.LB -> false.B, _.LBU -> false.B, _.LH -> false.B, _.LHU -> false.B, _.LW -> false.B,
          _.CSRRW -> false.B
        ),
        _.finish -> false.B,
        _.reOrderNum -> 0.U,
        _.regdes -> 1.U,
        _.regsrc1 -> 2.U,
        _.regsrc2 -> 3.U,
        _.pregsrc1 -> 4.U,
        _.pregsrc2 -> 5.U,
        _.pregdes -> 6.U,
        _.cmtdes -> 7.U,
        _.src1 -> 8.U,
        _.src2 -> 9.U,
        _.imm -> (new IMM).Lit(
          _.I -> 10.U, _.B -> 11.U, _.S -> 12.U, _.U -> 13.U, _.J -> 14.U, _.Z -> 15.U
        ),
        _.wbdata -> 16.U,
        _.jump -> (new JumpIssue).Lit(
          _.Valid -> false.B, _.actTarget -> 17.U, _.link -> 18.U
        ),
        _.branch -> (new BranchIssue).Lit(
          _.Valid -> false.B, _.actTaken -> false.B, _.target -> 19.U
        ),
        _.load -> (new LoadIssue).Lit(
          _.Valid -> false.B, _.addr -> 20.U, _.data -> 21.U, _.Ready -> false.B
        ),
        _.store -> (new StoreIssue).Lit(
          _.Valid -> false.B, _.addr -> 22.U, _.mask -> 0.U, _.data -> 23.U, _.Ready -> false.B
        ),
        _.csr_addr -> 24.U,
        _.csr_wdata -> 25.U
      )
      val instB = instA // 如需不同内容可再写一份
      val instC = instA

      c.io.DPRRA.poke(instA)
      c.io.DPRRB.poke(instB)
      c.io.DPRRC.poke(instC)
      c.io.FinC.poke((new InstCtrlBlock).Lit(
        _.Valid -> false.B, _.inst -> 0.U, _.pc -> 0.U,
        _.isa -> (new ISA).Lit(
          _.ADD -> false.B, _.ADDI -> false.B, _.SUB -> false.B, _.LUI -> false.B, _.AUIPC -> false.B,
          _.SLL -> false.B, _.SLLI -> false.B, _.SRL -> false.B, _.SRLI -> false.B, _.SRA -> false.B, _.SRAI -> false.B,
          _.XOR -> false.B, _.XORI -> false.B, _.OR -> false.B, _.ORI -> false.B, _.AND -> false.B, _.ANDI -> false.B,
          _.SLT -> false.B, _.SLTU -> false.B, _.SLTI -> false.B, _.SLTIU -> false.B,
          _.BEQ -> false.B, _.BNE -> false.B, _.BLT -> false.B, _.BGE -> false.B, _.BLTU -> false.B, _.BGEU -> false.B,
          _.JAL -> false.B, _.JALR -> false.B,
          _.SB -> false.B, _.SH -> false.B, _.SW -> false.B,
          _.LB -> false.B, _.LBU -> false.B, _.LH -> false.B, _.LHU -> false.B, _.LW -> false.B,
          _.CSRRW -> false.B
        ),
        _.finish -> false.B, _.reOrderNum -> 0.U, _.regdes -> 0.U, _.regsrc1 -> 0.U, _.regsrc2 -> 0.U,
        _.pregsrc1 -> 0.U, _.pregsrc2 -> 0.U, _.pregdes -> 0.U, _.cmtdes -> 0.U,
        _.src1 -> 0.U, _.src2 -> 0.U,
        _.imm -> (new IMM).Lit(_.I -> 0.U, _.B -> 0.U, _.S -> 0.U, _.U -> 0.U, _.J -> 0.U, _.Z -> 0.U),
        _.wbdata -> 0.U,
        _.jump -> (new JumpIssue).Lit(_.Valid -> false.B, _.actTarget -> 0.U, _.link -> 0.U),
        _.branch -> (new BranchIssue).Lit(_.Valid -> false.B, _.actTaken -> false.B, _.target -> 0.U),
        _.load -> (new LoadIssue).Lit(_.Valid -> false.B, _.addr -> 0.U, _.data -> 0.U, _.Ready -> false.B),
        _.store -> (new StoreIssue).Lit(_.Valid -> false.B, _.addr -> 0.U, _.mask -> 0.U, _.data -> 0.U, _.Ready -> false.B),
        _.csr_addr -> 0.U, _.csr_wdata -> 0.U
      ))
      c.io.FinD.poke(c.io.FinC.peek())
      c.io.FinE.poke(c.io.FinC.peek())
      c.io.Rollback.poke(false.B)
      c.io.bpuBranchAIdx.poke(0.U)
      c.io.bpuBranchBIdx.poke(0.U)

      c.clock.step(1)

      c.io.RREXA.Valid.expect(true.B)
      c.io.RREXB.Valid.expect(true.B)
      c.io.RREXC.Valid.expect(true.B)

      c.io.Rollback.poke(true.B)
      c.clock.step(1)
      c.io.RREXA.Valid.expect(false.B)
      c.io.RREXB.Valid.expect(false.B)
      c.io.RREXC.Valid.expect(false.B)
    }
  }
}