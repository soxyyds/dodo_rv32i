package DODO

import chisel3._
import chiseltest._
import org.scalatest._

class ExecuteTest extends org.scalatest.funsuite.AnyFunSuite with ChiselScalatestTester {
  test("Execute basic ADD + AND + SW") {
    test(new Execute) { dut =>
      // RREXA（ADD）
      dut.io.RREXA.Valid.poke(true.B)
      dut.io.RREXA.pc.poke(0x10.U)
      dut.io.RREXA.isa.ADD.poke(true.B)
      dut.io.RREXA.src1.poke(10.U)
      dut.io.RREXA.src2.poke(20.U)
      dut.io.RREXA.imm.I.poke(0.U)

      // RREXB（AND）
      dut.io.RREXB.Valid.poke(true.B)
      dut.io.RREXB.pc.poke(0x14.U)
      dut.io.RREXB.isa.AND.poke(true.B)
      dut.io.RREXB.src1.poke("hF0F0F0F0".U)
      dut.io.RREXB.src2.poke("h0F0F0F0F".U)
      dut.io.RREXB.imm.I.poke(0.U)

      // RREXC（SW）
      dut.io.RREXC.Valid.poke(true.B)
      dut.io.RREXC.pc.poke(0x18.U)
      dut.io.RREXC.isa.SW.poke(true.B)
      dut.io.RREXC.src1.poke(0x1000.U)
      dut.io.RREXC.src2.poke("hDEADBEEF".U)
      dut.io.RREXC.imm.S.poke(0x04.U)

      dut.io.Rollback.poke(false.B)

      dut.clock.step()

      // 基础断言
      assert(dut.io.FinC.wbdata.peek().litValue == 30)
      assert(dut.io.FinD.wbdata.peek().litValue == 0)
    }
  }
}
