package DODO

import DODO.BPU.Const.ADDR_WIDTH
import chisel3._
import chisel3.util._
import DODO.BPU._ //导入分支预测器相关模块

class InstDecode extends Module {
  val io = IO(new Bundle{
    val IFIDA = Input(new InstCtrlBlock)  // 指令A输入
    val IFIDB = Input(new InstCtrlBlock)  // 指令B输入
    val IDRMA = Output(new InstCtrlBlock) // 解码后指令A输出
    val IDRMB = Output(new InstCtrlBlock) // 解码后指令B输出

    val FetchBlock = Input(Bool())       // 取指阻塞信号
    val Rollback = Input(Bool())         // 流水线回滚信号

    // 新增的BP接口
    val branchInfo = new Bundle {
      val target = Output(UInt(ADDR_WIDTH.W))  // 从ID中取出指令的目标地址
      val branch = Output(Bool())              // 该指令是否为分支指令
      val jump = Output(Bool())                // 该指令是否为跳转指令
      val pc = Output(UInt(ADDR_WIDTH.W))      // 当前指令的pc值，用于更新BTB中的信息
    }
    val lookupPc = Output(UInt(ADDR_WIDTH.W))  // 想要查询的pc值，用于提前分支预测
  })

  // 流水线寄存器
  val RegA = Reg(new InstCtrlBlock)
  val RegB = Reg(new InstCtrlBlock)

  when(!io.FetchBlock) {
    RegA := io.IFIDA
    RegB := io.IFIDB
  }

  // 指令选择逻辑
  val INSTA = Mux(!io.FetchBlock, RegA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  val INSTB = Mux(!io.FetchBlock, RegB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))

  // 解码器实例
  val decodeA = Module(new Decoder)
  val decodeB = Module(new Decoder)
  decodeA.io.inst := INSTA.inst
  decodeB.io.inst := INSTB.inst

  // BP接口连接
  io.branchInfo.target := INSTA.pc + decodeA.io.imm.J  // 目标地址计算
  io.branchInfo.branch := decodeA.io.isa.Bclass()      // 是否为分支指令
  io.branchInfo.jump := decodeA.io.isa.Jclass()        // 是否为跳转指令
  io.branchInfo.pc := INSTA.pc                         // 当前指令PC

  io.lookupPc := INSTA.pc  // 查询PC使用当前指令PC

  // 流水线控制
  when(io.Rollback) {
    RegA := 0.U.asTypeOf(new InstCtrlBlock)
    RegB := 0.U.asTypeOf(new InstCtrlBlock)
    io.IDRMA := 0.U.asTypeOf(new InstCtrlBlock)
    io.IDRMB := 0.U.asTypeOf(new InstCtrlBlock)

    // Rollback时清除BP信号
    io.branchInfo.target := 0.U
    io.branchInfo.branch := false.B
    io.branchInfo.jump := false.B
    io.branchInfo.pc := 0.U
    io.lookupPc := 0.U
  }.otherwise {
    io.IDRMA := GenICB(
      decodeA.io.isa, decodeA.io.imm,
      decodeA.io.regdes, decodeA.io.regsrc1, decodeA.io.regsrc2,
      decodeA.io.csr_addr, INSTA
    )
    io.IDRMB := GenICB(
      decodeB.io.isa, decodeB.io.imm,
      decodeB.io.regdes, decodeB.io.regsrc1, decodeB.io.regsrc2,
      decodeB.io.csr_addr, INSTB
    )
  }

  // GenICB
  def GenICB(isa: ISA, imm: IMM, regdes: UInt, regsrc1: UInt, regsrc2: UInt,
             csr_addr: UInt, IFID: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB := IFID
    ICB.isa := isa
    ICB.regdes := regdes
    ICB.regsrc1 := regsrc1
    ICB.regsrc2 := regsrc2
    ICB.imm := imm
    ICB.csr_addr := csr_addr
    ICB
  }
}


class Decoder extends Module {
    val io = IO(new Bundle{
    val inst = Input(UInt(32.W))     // 32位指令输入
    val isa = Output(new ISA)        // 指令类型
    val imm = Output(new IMM)        // 立即数
    val regdes = Output(UInt(5.W))   // 目标寄存器
    val regsrc1 = Output(UInt(5.W))  // 源寄存器1
    val regsrc2 = Output(UInt(5.W))  // 源寄存器2
    val csr_addr = Output(UInt(12.W)) // CSR地址
  })

  // I-type指令 (立即数操作)
  io.isa.SLLI  := (io.inst === BitPat("b0000000_?????_?????_001_?????_0010011"))  // 逻辑左移立即数
  io.isa.SRLI  := (io.inst === BitPat("b0000000_?????_?????_101_?????_0010011"))  // 逻辑右移立即数
  io.isa.SRAI  := (io.inst === BitPat("b0100000_?????_?????_101_?????_0010011"))  // 算术右移立即数
  io.isa.ADDI  := (io.inst === BitPat("b????????????_?????_000_?????_0010011"))  // 加法立即数
  io.isa.XORI  := (io.inst === BitPat("b????????????_?????_100_?????_0010011"))  // 异或立即数
  io.isa.ORI   := (io.inst === BitPat("b????????????_?????_110_?????_0010011"))  // 或立即数
  io.isa.ANDI  := (io.inst === BitPat("b????????????_?????_111_?????_0010011"))  // 与立即数
  io.isa.SLTI  := (io.inst === BitPat("b????????????_?????_010_?????_0010011"))  // 有符号比较立即数
  io.isa.SLTIU := (io.inst === BitPat("b????????????_?????_011_?????_0010011"))  // 无符号比较立即数
  io.isa.JALR  := (io.inst === BitPat("b????????????_?????_000_?????_1100111"))  // 跳转并链接寄存器

  // 加载指令
  io.isa.LB    := (io.inst === BitPat("b????????????_?????_000_?????_0000011"))  // 加载字节
  io.isa.LH    := (io.inst === BitPat("b????????????_?????_001_?????_0000011"))  // 加载半字
  io.isa.LW    := (io.inst === BitPat("b????????????_?????_010_?????_0000011"))  // 加载字
  io.isa.LBU   := (io.inst === BitPat("b????????????_?????_100_?????_0000011"))  // 加载无符号字节
  io.isa.LHU   := (io.inst === BitPat("b????????????_?????_101_?????_0000011"))  // 加载无符号半字

  // R-type指令 (寄存器-寄存器操作)
  io.isa.ADD   := (io.inst === BitPat("b0000000_?????_?????_000_?????_0110011"))  // 加法
  io.isa.SUB   := (io.inst === BitPat("b0100000_?????_?????_000_?????_0110011"))  // 减法
  io.isa.SLL   := (io.inst === BitPat("b0000000_?????_?????_001_?????_0110011"))  // 逻辑左移
  io.isa.SLT   := (io.inst === BitPat("b0000000_?????_?????_010_?????_0110011"))  // 有符号比较
  io.isa.SLTU  := (io.inst === BitPat("b0000000_?????_?????_011_?????_0110011"))  // 无符号比较
  io.isa.XOR   := (io.inst === BitPat("b0000000_?????_?????_100_?????_0110011"))  // 异或
  io.isa.SRL   := (io.inst === BitPat("b0000000_?????_?????_101_?????_0110011"))  // 逻辑右移
  io.isa.SRA   := (io.inst === BitPat("b0100000_?????_?????_101_?????_0110011"))  // 算术右移
  io.isa.OR    := (io.inst === BitPat("b0000000_?????_?????_110_?????_0110011"))  // 或
  io.isa.AND   := (io.inst === BitPat("b0000000_?????_?????_111_?????_0110011"))  // 与

  // B-type指令 (分支指令)
  io.isa.BEQ   := (io.inst === BitPat("b???????_?????_?????_000_?????_1100011"))  // 相等分支
  io.isa.BNE   := (io.inst === BitPat("b???????_?????_?????_001_?????_1100011"))  // 不等分支
  io.isa.BLT   := (io.inst === BitPat("b???????_?????_?????_100_?????_1100011"))  // 小于分支
  io.isa.BGE   := (io.inst === BitPat("b???????_?????_?????_101_?????_1100011"))  // 大于等于分支
  io.isa.BLTU  := (io.inst === BitPat("b???????_?????_?????_110_?????_1100011"))  // 无符号小于分支
  io.isa.BGEU  := (io.inst === BitPat("b???????_?????_?????_111_?????_1100011"))  // 无符号大于等于分支

  // S-type指令 (存储指令)
  io.isa.SB    := (io.inst === BitPat("b???????_?????_?????_000_?????_0100011"))  // 存储字节
  io.isa.SH    := (io.inst === BitPat("b???????_?????_?????_001_?????_0100011"))  // 存储半字
  io.isa.SW    := (io.inst === BitPat("b???????_?????_?????_010_?????_0100011"))  // 存储字

  // J-type指令 (跳转指令)
  io.isa.JAL   := (io.inst === BitPat("b?????????????????????_?????_1101111"))  // 跳转并链接

  // U-type指令 (长立即数指令)
  io.isa.LUI   := (io.inst === BitPat("b?????????????????????_?????_0110111"))  // 加载高位立即数
  io.isa.AUIPC := (io.inst === BitPat("b?????????????????????_?????_0010111"))  // PC加立即数

  // 立即数提取
  val I = io.inst(31,20)  // I-type
  val B = Cat(io.inst(31), io.inst(7), io.inst(30,25), io.inst(11,8), 0.U(1.W)) // B-type
  val S = Cat(io.inst(31,25), io.inst(11,7)) // S-type
  val U = io.inst(31,12)  // U-type（不需要填充0）
  val J = Cat(io.inst(31), io.inst(19,12), io.inst(20), io.inst(30,21), 0.U(1.W)) // J-type

  // 32位立即数扩展
  io.imm.I := SignExt(I, 32)
  io.imm.B := SignExt(B, 32)
  io.imm.S := SignExt(S, 32)
  io.imm.U := U  //
  io.imm.J := SignExt(J, 32)

  // 识别CSRRW指令 (opcode=SYSTEM, func3=001)
  io.isa.CSRRW := (io.inst(6,0) === "b1110011".U) &&
    (io.inst(14,12) === "b001".U)
  // CSR地址
  io.csr_addr := io.inst(31,20)

  // 寄存器操作数识别
  val wen = io.isa.Aclass || io.isa.Jclass || io.isa.Lclass || io.isa.CSRRW
  val src1 =  (io.isa.Aclass() && !(io.isa.LUI || io.isa.AUIPC)) ||
    io.isa.Bclass() ||
    io.isa.Sclass() ||
    io.isa.Lclass() ||
    io.isa.JALR/* 识别需要rs1的指令 */
  val src2 =(io.isa.Aclass() &&
    ( io.isa.ADD || io.isa.SUB ||
      io.isa.SLL || io.isa.SRL ||
      io.isa.AND || io.isa.OR ||
      io.isa.SLT || io.isa.SLTU)) ||
      io.isa.Bclass() ||
      io.isa.Sclass() /* 识别需要rs2的指令 *///src2和src1并不都是存在的，因此我们需要使能信号来决定src是否有必要存在
      io.regdes := Mux(wen, io.inst(11,7), 0.U)
      io.regsrc1 := Mux(src1, io.inst(19,15), 0.U)
      io.regsrc2 := Mux(src2, io.inst(24,20), 0.U)
}

object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if(aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    if(aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}