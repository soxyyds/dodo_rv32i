package DODO
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MemoryTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory Module"

  it should "correctly load a word using LW" in {
    test(new Memory).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // 模拟初始化 EXMEM 输入（伪代码，实际需完整 InstCtrlBlock 定义）
      dut.io.EXMEM.load.addr.poke("h80000000".U)
      dut.io.EXMEM.isa.LW.poke(true.B)
      dut.io.EXMEM.Valid.poke(true.B)

      // 64位原始内存数据（模拟 memory 返回值）
      dut.io.DataRam.data_rdata.poke("h123456789abcdef0".U)

      dut.io.ForwardStore.Valid.poke(false.B)
      dut.io.CmtA.store.Valid.poke(false.B)
      dut.io.Rollback.poke(false.B)

      dut.clock.step(1)

      // 检查结果
      dut.io.ForwardLoad.Valid.expect(true.B)
      dut.io.ForwardLoad.data.expect("hffffffff9abcdef0".U)
    }
  }
}