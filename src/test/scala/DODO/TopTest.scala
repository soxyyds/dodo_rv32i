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
        val fetch_instA = dut.io.fetch_instA.peek().litValue()
        val fetch_instB = dut.io.fetch_instB.peek().litValue()
        val map_instA = dut.io.regMap_instA.peek().litValue()
        val map_instB = dut.io.regMap_instB.peek().litValue()
        val dis_instA = dut.io.dis_instA.peek().litValue()
        val dis_instB = dut.io.dis_instB.peek().litValue()
        val dis_instC = dut.io.dis_instC.peek().litValue()
        val fetchblock =dut.io.fetchblock.peek().litValue()
        val com_jumppcA =dut.io.com_jumppcA.peek().litValue()
        val com_jumppcB =dut.io.com_jumppcB.peek().litValue()
        val com_bpPredTargetA =dut.io.com_bpPredTargetA.peek().litValue()
        val com_bpPredTargetB =dut.io.com_bpPredTargetB.peek().litValue()
        val regMap_reg1 =  dut.io.regMap_reg1.peek().litValue()
        val regMap_reg2 =  dut.io.regMap_reg2.peek().litValue()
        val regMap_reg3 =  dut.io.regMap_reg3.peek().litValue()
        val regMap_reg4 =  dut.io.regMap_reg4.peek().litValue()
        val regMap_pre1 =  dut.io.regMap_pre1.peek().litValue()
        val regMap_pre3 =  dut.io.regMap_pre3.peek().litValue()
        val regMap_pre2 =  dut.io.regMap_pre2.peek().litValue()
        val regMap_pre4 =  dut.io.regMap_pre4.peek().litValue()
        val regMap_cmtdesA = dut.io.regMap_cmtdesA.peek().litValue()
        val regMap_cmtdesB = dut.io.regMap_cmtdesB.peek().litValue()
        val regMap_pregdesA = dut.io.regMap_pregdesA.peek().litValue()
        val regMap_pregdesB = dut.io.regMap_pregdesB.peek().litValue()
        val decode_reg1 = dut.io.decode_reg1.peek().litValue()
        val decode_reg2 = dut.io.decode_reg2.peek().litValue()
        val decode_reg3 = dut.io.decode_reg3.peek().litValue()
        val decode_reg4 = dut.io.decode_reg4.peek().litValue()
        val com_rollback =dut.io.com_rollback.peek().litValue()
        println(f"周期 $cycle: PC=0x${pc}%016X, InstA=0x${instA}%08X, InstB=0x${instB}%08X, fetch_instA=0x${fetch_instA}%08X,fetch_instB=0x${fetch_instB}%08X")
        println(f"map_instA=0x${map_instA}%08X,map_instB=0x${map_instB}%08X,dis_instA=0x${dis_instA}%08X,dis_instB=0x${dis_instB}%08X,dis_instC=0x${dis_instC}%08X,fetchblock=${fetchblock}%08X")
        println(f"跳转信号：com_jumppcA=0x${com_jumppcA}%08X,com_jumppcB=0x${com_jumppcB}%08X,com_bpPredTargetA=0x${com_bpPredTargetA}%08X,com_bpPredTargetB=0x${com_bpPredTargetB}%08X,Rollback=0x${com_rollback}%08X,")
        println(f"regmap信号：regMap_reg1=0x${regMap_reg1}%08X,regMap_reg2=0x${regMap_reg2}%08X,regMap_reg3=0x${regMap_reg3}%08X,regMap_reg4=0x${regMap_reg4}%08X,")
        println(f"regmap信号：regMap_pre1=0x${regMap_pre1}%08X,regMap_pre2=0x${regMap_pre2}%08X,regMap_pre3=0x${regMap_pre3}%08X,regMap_pre4=0x${regMap_pre4}%08X,")
        println(f"regmap信号：regMap_cmtdesA=0x${regMap_cmtdesA}%08X,regMap_cmtdesB=0x${regMap_cmtdesB}%08X,regMap_pregdesA=0x${regMap_pregdesA}%08X,regMap_pregdesB=0x${regMap_pregdesB}%08X,")
        println(f"regmap信号：decode_reg1=0x${decode_reg1}%08X,decode_reg2=0x${decode_reg2}%08X,decode_reg3=0x${decode_reg3}%08X,decode_reg4=0x${decode_reg4}%08X,")
        println(f"fin_A_jumptarget=0x${dut.io.fin_A_jumptarget.peek().litValue()}%016X, fin_A_branchtarget=0x${dut.io.fin_A_branchtarget.peek().litValue()}%016X,")
        println(f"fin_B_jumptarget=0x${dut.io.fin_B_jumptarget.peek().litValue()}%016X, fin_B_branchtarget=0x${dut.io.fin_B_branchtarget.peek().litValue()}%016X,")
        println(f"src1=0x${dut.io.src1.peek().litValue()}%08X, src2=0x${dut.io.src2.peek().litValue()}%08X, src3=0x${dut.io.src3.peek().litValue()}%08X, src4=0x${dut.io.src4.peek().litValue()}%08X,")
        println(f"read_instA=0x${dut.io.read_instA.peek().litValue()}%08X, read_instB=0x${dut.io.read_instB.peek().litValue()}%08X")
        dut.clock.step(1)
      }

      println("DHRYSTONE基准测试执行完成")
    }
  }
}
