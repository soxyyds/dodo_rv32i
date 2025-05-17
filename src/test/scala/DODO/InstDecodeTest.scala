//package DODO
//
//import chisel3._
//import chisel3.tester._
//import org.scalatest.FreeSpec
//import chisel3.experimental.BundleLiterals._
//
//class InstDecodeTest extends FreeSpec with ChiselScalatestTester {
//
//  // 测试用指令生成器
//  def genTestInst(
//                   inst: UInt,
//                   isaType: String,
//                   regdes: Int = 0,
//                   regsrc1: Int = 0,
//                   regsrc2: Int = 0,
//                   imm: Int = 0
//                 ): InstCtrlBlock = {
//    val icb = Wire(new InstCtrlBlock)
//
//    // 基础字段
//    icb.Valid := true.B
//    icb.inst := inst
//    icb.pc := 0x80000000.U  // 测试用PC值
//    icb.imm := imm.U
//
//    // 寄存器字段
//    icb.regdes := regdes.U(5.W)
//    icb.regsrc1 := regsrc1.U(5.W)
//    icb.regsrc2 := regsrc2.U(5.W)
//
//    // 初始化ISA类型
//    icb.isa := Wire(new ISA)
//    def initISA(): ISA = {
//      val isa = Wire(new ISA)
//
//      // Arithmetic
//      isa.ADD := false.B
//      isa.ADDI := false.B
//      isa.SUB := false.B
//      isa.LUI := false.B
//      isa.AUIPC := false.B
//
//      // Shift
//      isa.SLL := false.B
//      isa.SLLI := false.B
//      isa.SRL := false.B
//      isa.SRLI := false.B
//      isa.SRA := false.B
//      isa.SRAI := false.B
//
//      // Logic
//      isa.XOR := false.B
//      isa.XORI := false.B
//      isa.OR := false.B
//      isa.ORI := false.B
//      isa.AND := false.B
//      isa.ANDI := false.B
//
//      // Compare
//      isa.SLT := false.B
//      isa.SLTU := false.B
//      isa.SLTI := false.B
//      isa.SLTIU := false.B
//
//      // Branch
//      isa.BEQ := false.B
//      isa.BNE := false.B
//      isa.BLT := false.B
//      isa.BGE := false.B
//      isa.BLTU := false.B
//      isa.BGEU := false.B
//
//      // Jump
//      isa.JAL := false.B
//      isa.JALR := false.B
//
//      // Store
//      isa.SB := false.B
//      isa.SH := false.B
//      isa.SW := false.B
//
//      // Load
//      isa.LB := false.B
//      isa.LBU := false.B
//      isa.LH := false.B
//      isa.LHU := false.B
//      isa.LW := false.B
//
//      // CSR
//      isa.CSRRW := false.B
//
//      isa
//    }
//    isaType match {
//      case "ADDI"  => icb.isa.ADDI := true.B
//      case "LUI"   => icb.isa.LUI := true.B
//      case "ADD"   => icb.isa.ADD := true.B
//      case "BEQ"   => icb.isa.BEQ := true.B
//      case "SW"    => icb.isa.SW := true.B
//      case "CSRRW" => icb.isa.CSRRW := true.B
//      case _       => throw new IllegalArgumentException(s"未知指令类型: $isaType")
//    }
//
//    // 其他字段置默认值
//    icb.finish := false.B
//    icb.reOrderNum := 0.U
//    icb.pregdes := 0.U
//    icb.pregsrc1 := 0.U
//    icb.pregsrc2 := 0.U
//    icb.cmtdes := 0.U
//    icb.src1 := 0.U
//    icb.src2 := 0.U
//    icb.wbdata := 0.U
//    icb.csr_addr := 0.U
//    icb.csr_wdata := 0.U
//    icb.jump := 0.U.asTypeOf(new JumpIssue)
//    icb.branch := 0.U.asTypeOf(new BranchIssue)
//    icb.load := 0.U.asTypeOf(new LoadIssue)
//    icb.store := 0.U.asTypeOf(new StoreIssue)
//
//    icb
//  }
//
//  "InstDecode核心功能测试" - {
//    // 测试1: 基础指令解码
//    "应正确解码I型指令(ADDI)" in {
//      test(new InstDecode) { dut =>
//        val testInst = genTestInst(
//          inst = "h00400093".U,  // addi x1, x0, 4
//          isaType = "ADDI",
//          regdes = 1,
//          regsrc1 = 0,
//          imm = 4
//        )
//
//        dut.io.IFIDA.poke(testInst)
//        dut.clock.step()
//
//        // 验证解码结果
//        dut.io.IDRMA.isa.ADDI.expect(true.B)
//        dut.io.IDRMA.regdes.expect(1.U)
//        dut.io.IDRMA.regsrc1.expect(0.U)
//        dut.io.IDRMA.imm.expect(4.U)
//      }
//    }
//
//    // 测试2: U型指令解码
//    "应正确解码U型指令(LUI)" in {
//      test(new InstDecode) { dut =>
//        val testInst = genTestInst(
//          inst = "h00001137".U,  // lui x2, 1
//          isaType = "LUI",
//          regdes = 2,
//          imm = 4096  // 1 << 12
//        )
//
//        dut.io.IFIDB.poke(testInst)
//        dut.clock.step()
//
//        dut.io.IDRMB.isa.LUI.expect(true.B)
//        dut.io.IDRMB.regdes.expect(2.U)
//        dut.io.IDRMB.imm.expect(4096.U)
//      }
//    }
//
//    // 测试3: 流水线控制
//    "FetchBlock应暂停指令传递" in {
//      test(new InstDecode) { dut =>
//        val normalInst = genTestInst("h00400093".U, "ADDI")
//
//        // 正常情况
//        dut.io.IFIDA.poke(normalInst)
//        dut.io.FetchBlock.poke(false.B)
//        dut.clock.step()
//        dut.io.IDRMA.Valid.expect(true.B)
//
//        // 阻塞情况
//        dut.io.FetchBlock.poke(true.B)
//        dut.clock.step()
//        dut.io.IDRMA.Valid.expect(false.B)
//      }
//    }
//
//    // 测试4: 双发射验证
//    "应支持双指令并行解码" in {
//      test(new InstDecode) { dut =>
//        val instA = genTestInst("h00400093".U, "ADDI")  // addi x1, x0, 4
//        val instB = genTestInst("h00508133".U, "ADD")   // add x2, x1, x5
//
//        dut.io.IFIDA.poke(instA)
//        dut.io.IFIDB.poke(instB)
//        dut.clock.step()
//
//        // 验证两条指令并行解码
//        dut.io.IDRMA.isa.ADDI.expect(true.B)
//        dut.io.IDRMB.isa.ADD.expect(true.B)
//        dut.io.IDRMA.regdes.expect(1.U)
//        dut.io.IDRMB.regdes.expect(2.U)
//      }
//    }
//
//    // 测试5: 异常情况处理
//    "应正确处理非法指令" in {
//      test(new InstDecode) { dut =>
//        val illegalInst = {
//          val icb = genTestInst("hFFFFFFFF".U, "ADDI")
//          icb.isa := 0.U.asTypeOf(new ISA)
//          icb
//        }
//
//        dut.io.IFIDA.poke(illegalInst)
//        dut.clock.step()
//        dut.io.IDRMA.Valid.expect(false.B)
//      }
//    }
//  }
//
//  "Decoder子模块测试" - {
//    "应正确识别CSR指令" in {
//      test(new InstDecode) { dut =>
//        val csrInst = genTestInst(
//          inst = "h34051073".U,  // csrrw x0, mscratch, x10
//          isaType = "CSRRW",
//          regsrc1 = 10,
//          imm = 0x340
//        )
//
//        dut.io.IFIDA.poke(csrInst)
//        dut.clock.step()
//
//        dut.io.IDRMA.isa.CSRRW.expect(true.B)
//        dut.io.IDRMA.csr_addr.expect(0x340.U)
//        dut.io.IDRMA.regsrc1.expect(10.U)
//      }
//    }
//  }
//}
