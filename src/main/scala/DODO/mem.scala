package DODO // 修改成自己项目的文件夹名称

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import firrtl.annotations.MemoryLoadFileType

class if_mem extends Bundle {
  val instAddr = Output(UInt(64.W))
}//地址输出

class mem_id(val instWidth: Int) extends Bundle {
  val inst = Output(Vec(instWidth, UInt(32.W)))
}//指令获取

class lsu_mem_c extends Bundle {
  val dataAddr  = Output(UInt(64.W))
  val writeEn   = Output(Bool())
  val writeData = Output(UInt(32.W))
  val func3     = Output(UInt(3.W))
}

class mem_lsu_c extends Bundle {
  val data = Output(UInt(32.W))
}

class mem(memDepth: Int, instWidth: Int) extends Module {
  val io = IO(new Bundle {
    val reset   = Input(Bool())
    val if_mem  = Flipped(new if_mem())
    val ex_mem  = Flipped(new lsu_mem_c)
    val mem_id  = new mem_id(instWidth)
    val mem_lsu = new mem_lsu_c
  })

  val memInside = Mem(memDepth, Vec(4, UInt(8.W)))

  // 只定义32位写入缓冲区
  val memWriteVec = Wire(Vec(4, UInt(8.W)))

  // 1. 首先基于地址对齐方式重排数据字节顺序
  val alignedData = Wire(UInt(32.W))
  alignedData := MuxLookup(io.ex_mem.dataAddr(1, 0), io.ex_mem.writeData, Seq(
    0.U -> io.ex_mem.writeData,
    1.U -> Cat(io.ex_mem.writeData(23, 0), io.ex_mem.writeData(31, 24)),
    2.U -> Cat(io.ex_mem.writeData(15, 0), io.ex_mem.writeData(31, 16)),
    3.U -> Cat(io.ex_mem.writeData(7, 0), io.ex_mem.writeData(31, 8))
  ))

  // 2. 拆分对齐后的数据
  for (i <- 0 until 4) {
    memWriteVec(i) := alignedData(i*8+7, i*8)
  }

  loadMemoryFromFile(memInside, "/src/main/ramdata/dhrystone/dhrystone.data", MemoryLoadFileType.Hex)

  // 内存地址计算（字对齐）
  val dataAddr   = Wire(UInt(32.W))
  val dataAddrP1 = Wire(UInt(32.W))
  dataAddr   := io.ex_mem.dataAddr(31,0) >> 2.U
  dataAddrP1 := dataAddr + 1.U

  // 关键修改点2: 指令读取逻辑改为组合读
  for (i <- 0 until instWidth) {
    io.mem_id.inst(i) := Mux(
      io.reset,
      0x13.U,  // NOP指令
      memInside(io.if_mem.instAddr(31,0) + i.U).reduce((acc, elem) => Cat(elem, acc))
    )
  }

  // 关键修改点3: 数据读取改为组合读
  val rawData = memInside(dataAddr).reduce((acc, elem) => Cat(elem, acc))

  // 根据地址低2位选择对应字节位置（修正后）
  val alignedWord = MuxLookup(io.ex_mem.dataAddr(1, 0), rawData, Seq(
    0.U -> rawData,
    1.U -> Cat(rawData(7, 0), rawData(31, 8)),   // 正确取出15:8位到低8位
    2.U -> Cat(rawData(15, 0), rawData(31, 16)), // 保持不变
    3.U -> Cat(rawData(23, 0), rawData(31, 24))  // 保持不变
  ))

  // 根据func3进行数据宽度处理和符号扩展
  val processedData = Wire(UInt(32.W))
  // 首先提供默认值
  processedData := 0.U

  // 然后根据func3重载
  switch(io.ex_mem.func3) {
    is(0.U) { // LB - 带符号扩展的字节加载
      processedData := Cat(Fill(24,0.U), alignedWord(7, 0))
    }
    is(1.U) { // LH - 带符号扩展的半字加载
      processedData := Cat(Fill(16, 0.U), alignedWord(15, 0))
    }
    is(2.U) { // LW - 字加载
      processedData := alignedWord
    }
  }

  // 将处理后的数据输出
  io.mem_lsu.data := processedData

  // 内存写入掩码生成
  val memChoose = Wire(Vec(4, Bool()))
  for (i <- 0 to 3) {
    memChoose(i) := false.B
  }

  // 根据func3确定写入掩码
  switch(io.ex_mem.func3) {
    is(0.U) { // SB - 字节写入
      memChoose(io.ex_mem.dataAddr(1, 0)) := true.B
    }
    is(1.U) { // SH - 半字写入
      when(io.ex_mem.dataAddr(1, 0) === 0.U) {
        memChoose(0) := true.B
        memChoose(1) := true.B
      }.elsewhen(io.ex_mem.dataAddr(1, 0) === 1.U) {
        memChoose(1) := true.B
        memChoose(2) := true.B
      }.elsewhen(io.ex_mem.dataAddr(1, 0) === 2.U) {
        memChoose(2) := true.B
        memChoose(3) := true.B
      }.otherwise {
        memChoose(3) := true.B
        // 跨边界情况在32位系统中需特殊处理
      }
    }
    is(2.U) { // SW - 字写入
      memChoose(0) := true.B
      memChoose(1) := true.B
      memChoose(2) := true.B
      memChoose(3) := true.B
    }
  }

  // 执行内存写入
  when(io.ex_mem.writeEn) {
    // 监控特定地址的写入
    when(io.ex_mem.dataAddr === "h1001ff1".U) {
      val char = io.ex_mem.writeData(7, 0).asUInt
      printf("%c", char)  // 只打印对应字符
    }
    memInside.write(dataAddr, memWriteVec, memChoose)
  }
}

// 添加Verilog生成对象
object memVerilog extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(
    new mem(),
    args = Array(
      "-o", "mem.v",
      "--target-dir", "generated/mem",
      "--emission-options", "disableMemRandomization,disableRegisterRandomization"
    )
  )
}