package DODO

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopWithMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TopWithMemory"

  it should "执行DHRYSTONE基准测试" in {
    test(new TopWithMemory) { dut =>
      println("开始执行DHRYSTONE基准测试...")

      val maxCycles = 100
      for (cycle <- 1 to maxCycles) {
        // 直接从顶层IO访问PC和指令
        val pc = dut.io.pc.peek().litValue()
        val instA = dut.io.instA.peek().litValue()
        val instB = dut.io.instB.peek().litValue()

        println(f"周期 $cycle: PC=0x${pc}%016X, InstA=0x${instA}%08X, InstB=0x${instB}%08X")
        dut.clock.step(1)
      }

      println("DHRYSTONE基准测试执行完成")
    }
  }
}
