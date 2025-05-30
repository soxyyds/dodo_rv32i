package DODO

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TopwithMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TopWithMemory"

  it should "执行DHRYSTONE基准测试" in {
    test(new TopWithMemory) { dut =>
      dut.clock.setTimeout(0) // 禁用超时
      println("开始执行DHRYSTONE基准测试...")

      val maxCycles = 50000
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
        val fetchblock = dut.io.fetchblock.peek().litValue()
        val com_jumppcA = dut.io.com_jumppcA.peek().litValue()
        val com_jumppcB = dut.io.com_jumppcB.peek().litValue()
        val com_bpPredTargetA = dut.io.com_bpPredTargetA.peek().litValue()
        val com_bpPredTargetB = dut.io.com_bpPredTargetB.peek().litValue()
        val regMap_reg1 = dut.io.regMap_reg1.peek().litValue()
        val regMap_reg2 = dut.io.regMap_reg2.peek().litValue()
        val regMap_reg3 = dut.io.regMap_reg3.peek().litValue()
        val regMap_reg4 = dut.io.regMap_reg4.peek().litValue()
        val regMap_pre1 = dut.io.regMap_pre1.peek().litValue()
        val regMap_pre3 = dut.io.regMap_pre3.peek().litValue()
        val regMap_pre2 = dut.io.regMap_pre2.peek().litValue()
        val regMap_pre4 = dut.io.regMap_pre4.peek().litValue()
        val regMap_cmtdesA = dut.io.regMap_cmtdesA.peek().litValue()
        val regMap_cmtdesB = dut.io.regMap_cmtdesB.peek().litValue()
        val regMap_pregdesA = dut.io.regMap_pregdesA.peek().litValue()
        val regMap_pregdesB = dut.io.regMap_pregdesB.peek().litValue()
        val decode_reg1 = dut.io.decode_reg1.peek().litValue()
        val decode_reg2 = dut.io.decode_reg2.peek().litValue()
        val decode_reg3 = dut.io.decode_reg3.peek().litValue()
        val decode_reg4 = dut.io.decode_reg4.peek().litValue()
        val com_rollback = dut.io.com_rollback.peek().litValue()
        //      val com_reorderNumA = dut.io.com_reorderNumA.peek().litValue()
        //      val com_reorderNumB = dut.io.com_reorderNumB.peek().litValue()
        var last_fetchblock = 0
       var now_fetchblock = 0
        now_fetchblock = fetchblock.toInt
 //       if (last_fetchblock ==0  &&  now_fetchblock == 1) {

  //        println(f"周期 $cycle: PC=0x${pc}%016X, InstA=0x${instA}%08X, InstB=0x${instB}%08X,fetchblock=${fetchblock}%08X,")
//          println(f"map_instA=0x${map_instA}%08X,map_instB=0x${map_instB}%08X,dis_instA=0x${dis_instA}%08X,dis_instB=0x${dis_instB}%08X,dis_instC=0x${dis_instC}%08X,fetchblock=${fetchblock}%08X,")
          //     println(f"跳转信号：com_bpPredTargetA=0x${com_bpPredTargetA}%08X,com_bpPredTargetB=0x${com_bpPredTargetB}%08X,Rollback=0x${com_rollback}%08X,")
          //      println(f"com_bpPredtakenA=0x${dut.io.com_bpPredTakenA.peek().litValue()}%08X, com_bpPredTakenB=0x${dut.io.com_bpPredTakenB.peek().litValue()}%08X,")
          //     println(f"com_branchtakenA=0x${dut.io.com_branchtakenA.peek().litValue()}%08X, com_branchtakenB=0x${dut.io.com_branchtakenB.peek().litValue()}%08X,com_branchtargetA=0x${dut.io.com_branchtargetA.peek().litValue()}%08X, com_branchtargetB=0x${dut.io.com_branchtargetB.peek().litValue()}%08X,")
          //   println(f"：regMap_reg1=0x${regMap_reg1}%08X,regMap_reg2=0x${regMap_reg2}%08X,regMap_reg3=0x${regMap_reg3}%08X,regMap_reg4=0x${regMap_reg4}%08X,")
          //   println(f"：regMap_pre1=0x${regMap_pre1}%08X,regMap_pre2=0x${regMap_pre2}%08X,regMap_pre3=0x${regMap_pre3}%08X,regMap_pre4=0x${regMap_pre4}%08X,")
          //   println(f"：regMap_cmtdesA=0x${regMap_cmtdesA}%08X,regMap_cmtdesB=0x${regMap_cmtdesB}%08X,regMap_pregdesA=0x${regMap_pregdesA}%08X,regMap_pregdesB=0x${regMap_pregdesB}%08X,")
          //  println(f":regMap_regdesA=0x${dut.io.regMap_regdesA.peek().litValue()}%08X, regMap_regdesB=0x${dut.io.regMap_regdesB.peek().litValue()}%08X")
          //  println(f"：decode_reg1=0x${decode_reg1}%08X,decode_reg2=0x${decode_reg2}%08X,decode_reg3=0x${decode_reg3}%08X,decode_reg4=0x${decode_reg4}%08X,")
          //          println(f"map_inst=0x${map_instA}%016X,regMap_pregdesA=0x${regMap_pregdesA}%08X")
          //         println(f"map_instB=0x${map_instB}%016X,regMap_pregdesB=0x${regMap_pregdesB}%08X")
          //         println(f"")
          //    println(f"fin_A=0x${dut.io.fin_A_inst.peek().litValue()}%016X,finA_wbdata=0x${dut.io.fin_A_wbdata.peek().litValue()}%016X, fin_A_predes=0x${dut.io.finA_pregdes.peek().litValue()}%016X")
          //     println(f"fin_B=0x${dut.io.fin_B_inst.peek().litValue()}%016X,finB_wbdata=0x${dut.io.fin_B_wbdata.peek().litValue()}%016X, fin_B_predes=0x${dut.io.finB_pregdes.peek().litValue()}%016X")
          //    println(f"fin_C=0x${dut.io.fin_C_inst.peek().litValue()}%016X,finC_wbdata=0x${dut.io.fin_C_wbdata.peek().litValue()}%016X, fin_C_predes=0x${dut.io.finC_pregdes.peek().litValue()}%016X")
          //    println(f"fin_D=0x${dut.io.fin_D_inst.peek().litValue()}%016X,finD_wbdata=0x${dut.io.fin_D_wbdata.peek().litValue()}%016X, fin_D_predes=0x${dut.io.finD_pregdes.peek().litValue()}%016X")
          //    println(f"fin_E=0x${dut.io.fin_E_inst.peek().litValue()}%016X,finE_wbdata=0x${dut.io.fin_E_wbdata.peek().litValue()}%016X, fin_E_predes=0x${dut.io.finE_pregdes.peek().litValue()}%016X")
          //   println(f"")
          // println(f"fin_B_jumptarget=0x${dut.io.fin_B_jumptarget.peek().litValue()}%016X, fin_B_branchtarget=0x${dut.io.fin_B_branchtarget.peek().litValue()}%016X,")
  //        println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X, src1=0x${dut.io.src1.peek().litValue()}%08X, src2=0x${dut.io.src2.peek().litValue()}%08X, src3=0x${dut.io.src3.peek().litValue()}%08X, src4=0x${dut.io.src4.peek().litValue()}%08X")
            if(dut.io.mem_inst.peek().litValue !=0) {
              println(f"mem_inst=0x${dut.io.mem_inst.peek().litValue()}%08X, mem_wdata=0x${dut.io.mem_writeData.peek().litValue()}%08X, mem_fun3_write=0x${dut.io.mem_func3_write.peek().litValue()}%08X, mem_fun3_write=0x${dut.io.mem_func3_write.peek().litValue()}%08X,mem_addr=0x${dut.io.writeAddr.peek().litValue()}%08X, mem_writeEnable=${dut.io.mem_writeEnable.peek().litValue()},mem_readaddr=0x${dut.io.mem_readAddr.peek().litValue()}%08X,mem_rdata=0x${dut.io.mem_rdata.peek().litValue()}%08X,mem_fwdata=0x${dut.io.mem_fwdata.peek().litValue()}%08X,read_func3=0x${dut.io.read_func3.peek().litValue()}%08X")
          //    for(i <-0 to 63){ if(dut.io.com_bank(i).Valid.peek().litValue() ==1 && dut.io.com_bank(i).finish.peek().litValue() !=1){ println(f"com_bank_inst(${i})=0x${dut.io.com_bank(i).inst.peek().litValue()}%08X,com_bank_finish(${i})=0x${dut.io.com_bank(i).finish.peek().litValue()}%08X")}}
          //    println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instA=0x${dut.io.com_instA.peek().litValue()}%08X,com_dataA=0x${dut.io.com_dataA.peek().litValue()}%08X,com_jumppcA=0x${com_jumppcA}%08X,com_branchtargetA=0x${dut.io.com_branchtargetA.peek().litValue()}%08X,com_bpPredTargetA=0x${com_bpPredTargetA}%08X, com_bpPredTakenA=${dut.io.com_bpPredTakenA.peek().litValue()}, com_rollback=${com_rollback},com_jumptakenA=${dut.io.com_jumptakenA.peek().litValue()}, com_branchtakenA=${dut.io.com_branchtakenA.peek().litValue()}")
           //   println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instB=0x${dut.io.com_instB.peek().litValue()}%08X,com_dataB=0x${dut.io.com_dataB.peek().litValue()}%08X,com_jumppcB=0x${com_jumppcB}%08X,com_branchtargetB=0x${dut.io.com_branchtargetB.peek().litValue()}%08X,com_bpPredTargetB=0x${com_bpPredTargetB}%08X, com_bpPredTakenB=${dut.io.com_bpPredTakenB.peek().litValue()}, com_rollback=${com_rollback}, com_jumptakenA=${dut.io.com_jumptakenB.peek().litValue()}, com_branchtakenA=${dut.io.com_branchtakenB.peek().litValue()}")

            }
  //        println(f"com_EnQueuePointer=0x${dut.io.com_EnQueuePointer.peek().litValue()}%08X,com_DeQueuePointer=0x${dut.io.com_DeQueuePointer.peek().litValue()}%08X, ")
       //   if (dut.io.com_instA.peek().litValue() != 0 && (dut.io.com_jumptakenA.peek().litValue() != 0 || dut.io.com_branchtakenA.peek().litValue != 0) ) {
      //     println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instA=0x${dut.io.com_instA.peek().litValue()}%08X,com_dataA=0x${dut.io.com_dataA.peek().litValue()}%08X,com_jumppcA=0x${com_jumppcA}%08X,com_branchtargetA=0x${dut.io.com_branchtargetA.peek().litValue()}%08X,com_bpPredTargetA=0x${com_bpPredTargetA}%08X, com_bpPredTakenA=${dut.io.com_bpPredTakenA.peek().litValue()}, com_rollback=${com_rollback},com_jumptakenA=${dut.io.com_jumptakenA.peek().litValue()}, com_branchtakenA=${dut.io.com_branchtakenA.peek().litValue()}")
        //    println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instA=0x${dut.io.com_instA.peek().litValue()}%08X,com_dataA=0x${dut.io.com_dataA.peek().litValue()}%08X")
     //    }
     //     if (dut.io.com_instB.peek().litValue() != 0  && (dut.io.com_jumptakenB.peek().litValue() != 0 || dut.io.com_branchtakenB.peek().litValue != 0)) {
     //      println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instB=0x${dut.io.com_instB.peek().litValue()}%08X,com_dataB=0x${dut.io.com_dataB.peek().litValue()}%08X,com_jumppcB=0x${com_jumppcB}%08X,com_branchtargetB=0x${dut.io.com_branchtargetB.peek().litValue()}%08X,com_bpPredTargetB=0x${com_bpPredTargetB}%08X, com_bpPredTakenB=${dut.io.com_bpPredTakenB.peek().litValue()}, com_rollback=${com_rollback}, com_jumptakenA=${dut.io.com_jumptakenB.peek().litValue()}, com_branchtakenA=${dut.io.com_branchtakenB.peek().litValue()}")
   //         println(f"pc=0x${dut.io.com_pc.peek().litValue()}%08X,com_instA=0x${dut.io.com_instA.peek().litValue()}%08X,com_dataA=0x${dut.io.com_dataA.peek().litValue()}%08X")

          //}
          //     println(f"com_reorderNumA=0x${com_reorderNumA}%08X, com_reorderNumB=0x${com_reorderNumB}%08X, com_rollback=${com_rollback}")
          //      println(f"")
          //  if(cycle >= 7400 && cycle <=12000) {
          // if(fetchblock ==1 && last_fetchblock ==0 ){
//               for(i <-0 to 63){ if(dut.io.com_bank(i).Valid.peek().litValue() ==1 && dut.io.com_bank(i).finish.peek().litValue() !=1){ println(f"com_bank_inst(${i})=0x${dut.io.com_bank(i).inst.peek().litValue()}%08X,com_bank_finish(${i})=0x${dut.io.com_bank(i).finish.peek().litValue()}%08X")}}
          //    }
          //       last_fetchblock = dut.io.fetchblock.peek().litValue().toInt
       //    println(f"com_dataA=0x${dut.io.com_dataA.peek().litValue()}%08X,com_dataB=0x${dut.io.com_dataB.peek().litValue()}%08X,")
          //  }
          //         for(i <-0 to 0){ if(dut.io.com_bank(i).Valid.peek().litValue() ==1){ println(f"com_bank_inst(${i})=0x${dut.io.com_bank(i).inst.peek().litValue()}%08X,com_bank_finish(${i})=0x${dut.io.com_bank(i).finish.peek().litValue()}%08X")}}
          //         println(f"")
          //         println(f"presrc1=0x${dut.io.com_presrc1.peek().litValue()}%08X, pregsrc2=0x${dut.io.com_presrc2.peek().litValue()}%08X, pregsrc3=0x${dut.io.com_presrc3.peek().litValue()}%08X, pregsrc4=0x${dut.io.com_presrc4.peek().litValue()}%08X")
          //          println(f"pregdesA=0x${dut.io.com_predesA.peek().litValue()}%08X, pregdesB=0x${dut.io.com_predesB.peek().litValue()}%08X, cmtdesA=0x${dut.io.com_cmtdesA.peek().litValue()}%08X, cmtdesB=0x${dut.io.com_cmtdesB.peek().litValue()}%08X")
          //          println(f"")

          //  println(f"read_instA=0x${dut.io.read_instA.peek().litValue()}%08X, read_instB=0x${dut.io.read_instB.peek().litValue()}%08X")
          //  println(f"fin_C_wbdata=0x${dut.io.fin_C_wdata.peek().litValue()}%08X, fin_C_finish=${dut.io.fin_C_finish.peek().litValue()},fin_C_predes=${dut.io.fin_C_pregdes.peek().litValue()}")
          //  }

 //       }
        dut.clock.step(1) // 让时钟前进一步
     //   last_fetchblock = now_fetchblock
      }
    }
      println("DHRYSTONE基准测试执行完成")
  }
}
