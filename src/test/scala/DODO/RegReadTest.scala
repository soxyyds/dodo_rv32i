package DODO

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import DODO._

class RegReadTest extends AnyFlatSpec with ChiselScalatestTester {
  "RegRead" should "pass basic register read and rollback test" in {
    test(new RegRead) { c =>
      // 构造简单的InstCtrlBlock输入
      val instA = WireInit(0.U.asTypeOf(new InstCtrlBlock()))
      val instB = WireInit(0.U.asTypeOf(new InstCtrlBlock()))
      val instC = WireInit(0.U.asTypeOf(new InstCtrlBlock()))
      instA.Valid := true.B
      instA.pregsrc1 := 1.U
      instA.pregsrc2 := 2.U
      instA.pregdes := 3.U
      instA.inst := "h00000013".U // addi x0, x0, 0

      instB.Valid := true.B
      instB.pregsrc1 := 4.U
      instB.pregsrc2 := 5.U
      instB.pregdes := 6.U
      instB.inst := "h00000013".U

      instC.Valid := true.B
      instC.pregsrc1 := 7.U
      instC.pregsrc2 := 8.U
      instC.pregdes := 9.U
      instC.inst := "h00000013".U

      // 输入指令
      c.io.DPRRA.poke(instA)
      c.io.DPRRB.poke(instB)
      c.io.DPRRC.poke(instC)
      c.io.FinC.poke(0.U.asTypeOf(new InstCtrlBlock()))
      c.io.FinD.poke(0.U.asTypeOf(new InstCtrlBlock()))
      c.io.FinE.poke(0.U.asTypeOf(new InstCtrlBlock()))
      c.io.Rollback.poke(false.B)
      c.io.bpuBranchA_index.poke(0.U)
      c.io.bpuBranchB_index.poke(0.U)

      c.clock.step(1)

      // 检查输出是否有效
      c.io.RREXA.Valid.expect(true.B)
      c.io.RREXB.Valid.expect(true.B)
      c.io.RREXC.Valid.expect(true.B)

      // 模拟回滚
      c.io.Rollback.poke(true.B)
      c.clock.step(1)
      c.io.RREXA.Valid.expect(false.B)
      c.io.RREXB.Valid.expect(false.B)
      c.io.RREXC.Valid.expect(false.B)
    }
  }
}