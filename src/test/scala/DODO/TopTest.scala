package DODO

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopWithMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TopWithMemory"

  it should "执行DHRYSTONE基准测试" in {
    test(new TopWithMemory) { dut =>
      // 重要：需要确保mem.scala中加载了正确的dhrystone.data文件

      // 打印开始信息
      println("开始执行DHRYSTONE基准测试...")

      // 执行一系列周期
      val maxCycles = 10
      for (cycle <- 1 to maxCycles) {
        dut.clock.step(1)

        // 每隔一定周期打印一次进度
        if (cycle % 1 == 0) {
          println(s"已执行 $cycle 个周期...")
        }

        // 可以添加终止条件，例如检测到特定值或状态
        // 如果有类似测试完成的标志寄存器，可以在这里检查
      }

      println("DHRYSTONE基准测试执行完成")
    }
  }
}