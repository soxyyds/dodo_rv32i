package DODO

import chisel3._
import chisel3.util._


class InstDecode extends Module {
  // 模块IO端口定义
  val io = IO(new Bundle{
    val IFIDA = Input(new InstCtrlBlock)  // 指令A的输入包，包含指令和控制信息
    val IFIDB = Input(new InstCtrlBlock)  // 指令B的输入包
    val IDRMA = Output(new InstCtrlBlock) // 解码后指令A的输出包
    val IDRMB = Output(new InstCtrlBlock) // 解码后指令B的输出包

    val FetchBlock = Input(Bool())       // 取指阻塞信号，为高时暂停流水线
    val Rollback = Input(Bool())         // 流水线回滚信号，为高时清空流水线寄存器
  })

  // 流水线寄存器 - 在非阻塞时锁存输入指令
  val RegA = RegEnable(io.IFIDA, ~io.FetchBlock)  // 指令A的流水线寄存器
  val RegB = RegEnable(io.IFIDB, ~io.FetchBlock)  // 指令B的流水线寄存器

  // 指令选择逻辑：阻塞时输出空指令(NOP)，否则输出寄存器中的指令
  val INSTA = Mux(~io.FetchBlock, RegA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  val INSTB = Mux(~io.FetchBlock, RegB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))

  // 实例化两个解码器，分别处理两条指令
  val decodeA = Module(new Decoder)  // 指令A解码器
  val decodeB = Module(new Decoder)  // 指令B解码器
  decodeA.io.inst <> INSTA.inst      // 连接指令A到解码器A
  decodeB.io.inst <> INSTB.inst      // 连接指令B到解码器B

  // 流水线控制逻辑
  when(io.Rollback) {
    // 回滚时清空所有流水线寄存器和输出
    RegA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    RegB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.IDRMA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.IDRMB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise {
    // 正常执行时生成解码后的控制块
    io.IDRMA := GenICB(
      decodeA.io.isa,      // 指令A的类型
      decodeA.io.imm,      // 指令A的立即数
      decodeA.io.regdes,   // 指令A的目标寄存器
      decodeA.io.regsrc1,  // 指令A的源寄存器1
      decodeA.io.regsrc2,  // 指令A的源寄存器2
      decodeA.io.csr_addr,  // 指令A的CSR地址(如果是CSR指令)
      INSTA                // 指令A的原始控制信息
    )
    io.IDRMB := GenICB(
      decodeB.io.isa,      // 指令B的类型
      decodeB.io.imm,      // 指令B的立即数
      decodeB.io.regdes,   // 指令B的目标寄存器
      decodeB.io.regsrc1,  // 指令B的源寄存器1
      decodeB.io.regsrc2,  // 指令B的源寄存器2
      decodeB.io.csr_addr,  // 指令B的CSR地址
      INSTB                // 指令B的原始控制信息
    )
  }


  def GenICB(isa: ISA, imm: IMM, regdes: UInt, regsrc1: UInt, regsrc2: UInt,
             csr_addr: UInt, IFID: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)  // 创建新的控制块

    // 复制原始控制信息
    ICB.Valid := IFID.Valid         // 指令有效标志
    ICB.inst := IFID.inst           // 原始指令字
    ICB.pc := IFID.pc               // 程序计数器值
    ICB.finish := IFID.finish       // 指令完成标志
    ICB.reOrderNum := IFID.reOrderNum // 重排序缓冲区编号
    ICB.pregsrc1 := IFID.pregsrc1   // 物理源寄存器1
    ICB.pregsrc2 := IFID.pregsrc2   // 物理源寄存器2
    ICB.pregdes := IFID.pregdes     // 物理目标寄存器
    ICB.cmtdes := IFID.cmtdes       // 提交目标寄存器
    ICB.src1 := IFID.src1           // 源操作数1值
    ICB.src2 := IFID.src2           // 源操作数2值
    ICB.wbdata := IFID.wbdata       // 写回数据
    ICB.jump := IFID.jump           // 跳转指令标志
    ICB.branch := IFID.branch       // 分支指令标志
    ICB.load := IFID.load           // 加载指令标志
    ICB.store := IFID.store         // 存储指令标志

    // 添加解码结果
    ICB.isa := isa                  // 指令类型
    ICB.regdes := regdes            // 目标寄存器编号
    ICB.regsrc1 := regsrc1          // 源寄存器1编号
    ICB.regsrc2 := regsrc2          // 源寄存器2编号
    ICB.imm := imm                  // 立即数
    ICB.csr_addr := csr_addr        // CSR地址(如果是CSR指令)

    ICB  // 返回生成的控制块
  }
}


class Decoder extends Module {
  // 模块IO端口定义
  val io = IO(new Bundle{
    val inst = Input(UInt(32.W))     // 32位指令输入
    val isa = Output(new ISA)        // 指令类型输出
    val imm = Output(new IMM)        // 立即数输出
    val regdes = Output(UInt(5.W))   // 目标寄存器编号(rd)
    val regsrc1 = Output(UInt(5.W))  // 源寄存器1编号(rs1)
    val regsrc2 = Output(UInt(5.W))  // 源寄存器2编号(rs2)
    val csr_addr = Output(UInt(12.W)) // CSR地址输出
  })

  // I-type指令识别
  io.isa.SLLI     := (io.inst === BitPat("b000000??????_?????_001_?????_0010011"))  // 逻辑左移立即数
  io.isa.SLLIW    := (io.inst === BitPat("b000000??????_?????_001_?????_0011011"))  // 逻辑左移立即数(32位)
  io.isa.SRLI     := (io.inst === BitPat("b000000??????_?????_101_?????_0010011"))  // 逻辑右移立即数
  io.isa.SRLIW    := (io.inst === BitPat("b000000??????_?????_101_?????_0011011"))  // 逻辑右移立即数(32位)
  io.isa.SRAI     := (io.inst === BitPat("b010000??????_?????_101_?????_0010011"))  // 算术右移立即数
  io.isa.SRAIW    := (io.inst === BitPat("b010000??????_?????_101_?????_0011011"))  // 算术右移立即数(32位)
  io.isa.ADDI     := (io.inst === BitPat("b????????????_?????_000_?????_0010011"))  // 加法立即数
  io.isa.ADDIW    := (io.inst === BitPat("b????????????_?????_000_?????_0011011"))  // 加法立即数(32位)
  io.isa.XORI     := (io.inst === BitPat("b????????????_?????_100_?????_0010011"))  // 异或立即数
  io.isa.ORI      := (io.inst === BitPat("b????????????_?????_110_?????_0010011"))  // 或立即数
  io.isa.ANDI     := (io.inst === BitPat("b????????????_?????_111_?????_0010011"))  // 与立即数
  io.isa.SLTI     := (io.inst === BitPat("b????????????_?????_010_?????_0010011"))  // 有符号比较立即数
  io.isa.SLTIU    := (io.inst === BitPat("b????????????_?????_011_?????_0010011"))  // 无符号比较立即数
  io.isa.JALR     := (io.inst === BitPat("b????????????_?????_000_?????_1100111"))  // 跳转并链接寄存器

  // 加载指令
  io.isa.LD       := (io.inst === BitPat("b????????????_?????_011_?????_0000011"))  // 加载双字
  io.isa.LW       := (io.inst === BitPat("b????????????_?????_010_?????_0000011"))  // 加载字
  io.isa.LWU      := (io.inst === BitPat("b????????????_?????_110_?????_0000011"))  // 加载无符号字
  io.isa.LH       := (io.inst === BitPat("b????????????_?????_001_?????_0000011"))  // 加载半字
  io.isa.LHU      := (io.inst === BitPat("b????????????_?????_101_?????_0000011"))  // 加载无符号半字
  io.isa.LB       := (io.inst === BitPat("b????????????_?????_000_?????_0000011"))  // 加载字节
  io.isa.LBU      := (io.inst === BitPat("b????????????_?????_100_?????_0000011"))  // 加载无符号字节

  // R-type指令识别
  io.isa.SLL      := (io.inst === BitPat("b000000??????_?????_001_?????_0110011"))  // 逻辑左移
  io.isa.SLLW     := (io.inst === BitPat("b000000??????_?????_001_?????_0111011"))  // 逻辑左移(32位)
  io.isa.SRL      := (io.inst === BitPat("b000000??????_?????_101_?????_0110011"))  // 逻辑右移
  io.isa.SRLW     := (io.inst === BitPat("b000000??????_?????_101_?????_0111011"))  // 逻辑右移(32位)
  io.isa.SRA      := (io.inst === BitPat("b010000??????_?????_101_?????_0110011"))  // 算术右移
  io.isa.SRAW     := (io.inst === BitPat("b010000??????_?????_101_?????_0111011"))  // 算术右移(32位)
  io.isa.ADD      := (io.inst === BitPat("b000000??????_?????_000_?????_0110011"))  // 加法
  io.isa.ADDW     := (io.inst === BitPat("b000000??????_?????_000_?????_0111011"))  // 加法(32位)
  io.isa.SUB      := (io.inst === BitPat("b010000??????_?????_000_?????_0110011"))  // 减法
  io.isa.SUBW     := (io.inst === BitPat("b010000??????_?????_000_?????_0111011"))  // 减法(32位)
  io.isa.XOR      := (io.inst === BitPat("b000000??????_?????_100_?????_0110011"))  // 异或
  io.isa.OR       := (io.inst === BitPat("b000000??????_?????_110_?????_0110011"))  // 或
  io.isa.AND      := (io.inst === BitPat("b000000??????_?????_111_?????_0110011"))  // 与
  io.isa.SLT      := (io.inst === BitPat("b000000??????_?????_010_?????_0110011"))  // 有符号比较
  io.isa.SLTU     := (io.inst === BitPat("b000000??????_?????_011_?????_0110011"))  // 无符号比较

  // B-type分支指令识别
  io.isa.BEQ      := (io.inst === BitPat("b???????_?????_?????_000_?????_1100011"))  // 相等分支
  io.isa.BNE      := (io.inst === BitPat("b???????_?????_?????_001_?????_1100011"))  // 不等分支
  io.isa.BLT      := (io.inst === BitPat("b???????_?????_?????_100_?????_1100011"))  // 小于分支
  io.isa.BGE      := (io.inst === BitPat("b???????_?????_?????_101_?????_1100011"))  // 大于等于分支
  io.isa.BLTU     := (io.inst === BitPat("b???????_?????_?????_110_?????_1100011"))  // 无符号小于分支
  io.isa.BGEU     := (io.inst === BitPat("b???????_?????_?????_111_?????_1100011"))  // 无符号大于等于分支

  // S-type存储指令
  io.isa.SD       := (io.inst === BitPat("b???????_?????_?????_011_?????_0100011"))  // 存储双字
  io.isa.SW       := (io.inst === BitPat("b???????_?????_?????_010_?????_0100011"))  // 存储字
  io.isa.SH       := (io.inst === BitPat("b???????_?????_?????_001_?????_0100011"))  // 存储半字
  io.isa.SB       := (io.inst === BitPat("b???????_?????_?????_000_?????_0100011"))  // 存储字节

  // J/U-type指令
  io.isa.LUI      := (io.inst === BitPat("b?????????????????????_?????_0110111"))  // 加载高位立即数
  io.isa.AUIPC    := (io.inst === BitPat("b?????????????????????_?????_0010111"))  // PC加立即数
  io.isa.JAL      := (io.inst === BitPat("b?????????????????????_?????_1101111"))  // 跳转并链接

  // CSR指令
  io.isa.CSRRW    := (io.inst(6,0) === "b1110011".U) && (io.inst(14,12) === "b001".U)  // CSRRW指令

  // 提取不同格式的立即数
  val I = io.inst(31,20)  // I-type立即数(12位)
  val B = Cat(io.inst(31), io.inst(7), io.inst(30,25), io.inst(11,8), 0.U(1.W))  // B-type立即数(13位)
  val S = Cat(io.inst(31,25), io.inst(11,7))  // S-type立即数(12位)
  val U = Cat(io.inst(31,12), 0.U(12.W))  // U-type立即数(32位)
  val J = Cat(io.inst(31), io.inst(19,12), io.inst(20), io.inst(30,21), 0.U(1.W))  // J-type立即数(21位)
  val Z = io.inst(19,15)  // CSR指令的立即数(5位)

  // 立即数符号/零扩展
  io.imm.I := SignExt(I, 64)  // I-type符号扩展到64位
  io.imm.B := SignExt(B, 64)  // B-type符号扩展到64位
  io.imm.S := SignExt(S, 64)  // S-type符号扩展到64位
  io.imm.U := SignExt(U, 64)  // U-type符号扩展到64位
  io.imm.J := SignExt(J, 64)  // J-type符号扩展到64位
  io.imm.Z := ZeroExt(Z, 64)  // Z-type零扩展到64位

  // 提取CSR地址(指令的31:20位)
  io.csr_addr := io.inst(31,20)

  // 判断是否需要写寄存器(rd)
  val wen = io.isa.Aclass ||  // 算术指令
    io.isa.Jclass ||  // 跳转指令
    io.isa.Lclass ||  // 加载指令
    io.isa.CSRRW     // CSR指令

  // 判断是否需要源寄存器2(rs2)
  val src2 = io.isa.ADD || io.isa.ADDW || io.isa.SUB || io.isa.SUBW ||  // 算术运算
    io.isa.XOR || io.isa.OR || io.isa.AND ||                   // 逻辑运算
    io.isa.SLT || io.isa.SLTU ||                               // 比较指令
    io.isa.SLL || io.isa.SLLW || io.isa.SRL || io.isa.SRLW || io.isa.SRA || io.isa.SRAW ||  // 移位指令
    io.isa.BEQ || io.isa.BNE || io.isa.BLT || io.isa.BGE || io.isa.BLTU || io.isa.BGEU ||  // 分支指令
    io.isa.SD || io.isa.SW || io.isa.SH || io.isa.SB          // 存储指令

  // 判断是否需要源寄存器1(rs1)
  val src1 = src2 ||                                                     // 需要rs2的指令也需要rs1
    io.isa.ADDI || io.isa.ADDIW ||                              // 立即数加法
    io.isa.XORI || io.isa.ORI || io.isa.ANDI ||                 // 立即数逻辑运算
    io.isa.SLTI || io.isa.SLTIU ||                              // 立即数比较
    io.isa.SLLI || io.isa.SLLIW || io.isa.SRLI || io.isa.SRLIW || io.isa.SRAI || io.isa.SRAIW ||  // 立即数移位
    io.isa.JALR ||                                              // 跳转寄存器
    io.isa.LD || io.isa.LW || io.isa.LH || io.isa.LB || io.isa.LWU || io.isa.LHU || io.isa.LBU ||  // 加载指令
    io.isa.CSRRW                                                // CSR指令

  // 提取寄存器编号
  io.regdes := Mux(wen, io.inst(11,7), 0.U)     // rd在[11:7]，不需要时置0
  io.regsrc1 := Mux(src1, io.inst(19,15), 0.U)  // rs1在[19:15]，不需要时置0
  io.regsrc2 := Mux(src2, io.inst(24,20), 0.U)  // rs2在[24:20]，不需要时置0
}

//符号扩展工具
object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)  // 获取符号位
    if(aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)  // 符号扩展
  }
}

//零扩展工具
object ZeroExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    if(aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)  // 零扩展
  }
}