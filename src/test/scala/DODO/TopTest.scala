package DODO

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.io.{FileWriter, PrintWriter}

class TopwithMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TopWithMemory"

  it should "执行DHRYSTONE基准测试" in {
    // 创建输出文件
    val outputFile = new PrintWriter(new FileWriter("output"))

    test(new TopWithMemory) { dut =>
      dut.clock.setTimeout(0) // 禁用超时
      println("开始执行DHRYSTONE基准测试...")

      val maxCycles = 110000
      for (cycle <- 1 to maxCycles) {
        if(dut.io.writeEnable.peek().litValue == 1 && dut.io.writeAddr.peek().litValue == 0x10001ff1L) {
          val char = (dut.io.writeData.peek().litValue & 0xFF).toChar
          // 将字符写入文件
          outputFile.write(char)
          // 同时也在控制台显示
          print(char)
          // 确保及时写入文件
          outputFile.flush()
        }

        if(cycle % 10000 == 0) {
          println(s"周期cycle = 0x${cycle}%08X,")
        }

        dut.clock.step(1) // 让时钟前进一步
      }
    }

    // 关闭文件
    outputFile.close()
    println("DHRYSTONE基准测试执行完成")
  }
}