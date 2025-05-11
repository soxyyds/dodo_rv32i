package DODO

// 修改成自己项目的文件夹名称

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import firrtl.annotations.MemoryLoadFileType

class if_mem extends Bundle {
  val instAddr = Output(UInt(64.W))
}

class mem_id(val instWidth: Int) extends Bundle {
  val inst = Output(Vec(instWidth, UInt(32.W)))
}

class lsu_mem_c extends Bundle {
  val atomFlag  = Output(Bool())
  val dataAddr  = Output(UInt(64.W))
  val writeEn   = Output(Bool())
  val writeData = Output(UInt(64.W))
  val func3     = Output(UInt(3.W))
}

//class lsu_mem_c extends Bundle {
//  val atomFlag  = Output(Bool())
//  val dataAddr  = Output(UInt(64.W))
//  val writeEn   = Output(Bool())
//  val writeData = Output(UInt(32.W)) // 改为32位宽数据
//  val func3     = Output(UInt(3.W))
//}

class mem_lsu_c extends Bundle {
  val data = Output(UInt(64.W))
}

//class mem_lsu_c extends Bundle {
//  val data = Output(UInt(32.W)) // 改为32位宽数据
//}

class mem(memDepth: Int, instWidth: Int) extends Module {
  val io = IO(new Bundle {
    val reset   = Input(Bool())
    val if_mem  = Flipped(new if_mem())
    val ex_mem  = Flipped(new lsu_mem_c)
    val mem_id  = new mem_id(instWidth)
    val mem_lsu = new mem_lsu_c
  })

  val memInside = SyncReadMem(memDepth, Vec(4, UInt(8.W)))

  //  var memAddrDiff = Wire(UInt(2.W))
  //  for (i <- 0 to 3) {
  //    val memAddrDiff = i - io.ex_mem.dataAddr(1,0).litValue().toInt
  //    if (i >= io.ex_mem.dataAddr(1,0).litValue().toInt) {
  //      memWriteVec(i) := io.ex_mem.writeData(memAddrDiff * 8 + 8, memAddrDiff * 8)
  //    } else {
  //      memWriteVec(i) := io.ex_mem.writeData((32 - memAddrDiff * 8) + 8, 32 - memAddrDiff * 8)
  //    }
  //  }

  val memWriteVec  = Wire(Vec(4, UInt(8.W)))
  val memWriteVec1 = Wire(Vec(4, UInt(8.W)))
  for (i <- 0 to 3) {
    memWriteVec(i)  := io.ex_mem.writeData(i * 8 + 7, i * 8)
    memWriteVec1(i) := io.ex_mem.writeData((i + 4) * 8 + 7, (i + 4) * 8)
  }
  (0 until 8).map(_.asUInt)
  switch(io.ex_mem.dataAddr(1, 0)) {
    is(1.U) {
      for (i <- 0 to 2) {
        memWriteVec(i + 1)  := io.ex_mem.writeData(i * 8 + 7, i * 8)
        memWriteVec1(i + 1) := io.ex_mem.writeData((i + 4) * 8 + 7, (i + 4) * 8)
      }
      for (i <- 0 to 0) {
        memWriteVec(i)  := io.ex_mem.writeData((i + 3) * 8 + 7, (i + 3) * 8)
        memWriteVec1(i) := io.ex_mem.writeData((i + 7) * 8 + 7, (i + 7) * 8)
      }
    }
    is(2.U) {
      for (i <- 0 to 1) {
        memWriteVec(i + 2)  := io.ex_mem.writeData(i * 8 + 7, i * 8)
        memWriteVec1(i + 2) := io.ex_mem.writeData((i + 4) * 8 + 7, (i + 4) * 8)
      }
      for (i <- 0 to 1) {
        memWriteVec(i)  := io.ex_mem.writeData((i + 2) * 8 + 7, (i + 2) * 8)
        memWriteVec1(i) := io.ex_mem.writeData((i + 6) * 8 + 7, (i + 6) * 8)
      }
    }
    is(3.U) {
      for (i <- 0 to 0) {
        memWriteVec(i + 3)  := io.ex_mem.writeData(i * 8 + 7, i * 8)
        memWriteVec1(i + 3) := io.ex_mem.writeData((i + 4) * 8 + 7, (i + 4) * 8)
      }
      for (i <- 0 to 2) {
        memWriteVec(i)  := io.ex_mem.writeData((i + 1) * 8 + 7, (i + 1) * 8)
        memWriteVec1(i) := io.ex_mem.writeData((i + 5) * 8 + 7, (i + 5) * 8)
      }
    }
  }

  loadMemoryFromFile(memInside, ".../.../.../drystone.data", MemoryLoadFileType.Hex)

  val dataAddr   = Wire(UInt(64.W))
  val dataAddrP1 = Wire(UInt(64.W))
  val dataAddrP2 = Wire(UInt(64.W))
  dataAddr   := io.ex_mem.dataAddr >> 2.U
  dataAddrP1 := dataAddr + 1.U
  dataAddrP2 := dataAddr + 2.U

  for (i <- 0 until instWidth) {
    io.mem_id.inst(i) := Mux(
      io.reset,
      0x13.U,
      memInside.read(io.if_mem.instAddr + i.U).reduce((acc, elem) => Cat(elem, acc))
    )
  }
  io.mem_lsu.data := Cat(
    memInside.read(dataAddrP1).reduce((acc, elem) => Cat(elem, acc)),
    memInside.read(dataAddr).reduce((acc, elem) => Cat(elem, acc))
  )

  val memChoose0 = Wire(Vec(4, Bool()))
  val memChoose1 = Wire(Vec(4, Bool()))
  for (i <- 0 to 3) {
    memChoose0(i) := false.B
    memChoose1(i) := false.B
  }
  switch(io.ex_mem.func3) {
    is(0.U) { // SB
      memChoose0(io.ex_mem.dataAddr(1, 0)) := true.B
    }
    is(1.U) { // SH
      when(io.ex_mem.dataAddr(1, 0) =/= 3.U) {
        memChoose0(io.ex_mem.dataAddr(1, 0))       := true.B
        memChoose0(io.ex_mem.dataAddr(1, 0) + 1.U) := true.B
      }.otherwise {
        memChoose0(3) := true.B
        memChoose1(0) := true.B
      }
    }
    is(2.U) { // SW
      memChoose0(0) := Mux(io.ex_mem.dataAddr(1, 0) === 0.U, true.B, false.B)
      memChoose0(1) := Mux(io.ex_mem.dataAddr(1, 0) <= 1.U, true.B, false.B)
      memChoose0(2) := Mux(io.ex_mem.dataAddr(1, 0) <= 2.U, true.B, false.B)
      memChoose0(3) := Mux(io.ex_mem.dataAddr(1, 0) <= 3.U, true.B, false.B)
      memChoose1(0) := Mux(io.ex_mem.dataAddr(1, 0) >= 1.U, true.B, false.B)
      memChoose1(1) := Mux(io.ex_mem.dataAddr(1, 0) >= 2.U, true.B, false.B)
      memChoose1(2) := Mux(io.ex_mem.dataAddr(1, 0) >= 3.U, true.B, false.B)
    }
    is(3.U) { // SD
      memChoose0(0) := Mux(io.ex_mem.dataAddr(1, 0) === 0.U, true.B, false.B)
      memChoose0(1) := Mux(io.ex_mem.dataAddr(1, 0) <= 1.U, true.B, false.B)
      memChoose0(2) := Mux(io.ex_mem.dataAddr(1, 0) <= 2.U, true.B, false.B)
      memChoose0(3) := Mux(io.ex_mem.dataAddr(1, 0) <= 3.U, true.B, false.B)
      memChoose1(0) := Mux(io.ex_mem.dataAddr(1, 0) >= 1.U, true.B, false.B)
      memChoose1(1) := Mux(io.ex_mem.dataAddr(1, 0) >= 2.U, true.B, false.B)
      memChoose1(2) := Mux(io.ex_mem.dataAddr(1, 0) >= 3.U, true.B, false.B)
    }
  }

  when(io.ex_mem.writeEn) {
    when(io.ex_mem.func3 === 3.U) {
      memInside.write(dataAddr, memWriteVec, memChoose0)
      memInside.write(dataAddrP1, memWriteVec, memChoose1)
      memInside.write(dataAddrP1, memWriteVec1, memChoose0)
      memInside.write(dataAddrP2, memWriteVec1, memChoose1)
    }.otherwise {
      memInside.write(dataAddr, memWriteVec, memChoose0)
      memInside.write(dataAddrP1, memWriteVec, memChoose1)
    }
  }

}


//class mem_lsu_c extends Bundle {
//  val data = Output(UInt(32.W)) // 改为32位宽数据
//}
//
//class mem(memDepth: Int, instWidth: Int) extends Module {
//  val io = IO(new Bundle {
//    val reset   = Input(Bool())
//    val if_mem  = Flipped(new if_mem())
//    val ex_mem  = Flipped(new lsu_mem_c)
//    val mem_id  = new mem_id(instWidth)
//    val mem_lsu = new mem_lsu_c
//  })
//
//  // 直接存储32位数据，简化内存结构
//  val memInside = SyncReadMem(memDepth, UInt(32.W))
//  loadMemoryFromFile(memInside, ".../.../.../drystone.data", MemoryLoadFileType.Hex)
//
//  // 处理字节对齐
//  val addrAligned = Wire(UInt(64.W))
//  addrAligned := (io.ex_mem.dataAddr >> 2.U) << 2.U
//
//  // 处理不同长度的读取和写入
//  val byteOffset = io.ex_mem.dataAddr(1, 0)
//  val byteShift = byteOffset << 3.U
//
//  // 字节掩码（用于写入）
//  val byteMask = Wire(UInt(4.W))
//  byteMask := MuxLookup(
//    io.ex_mem.func3,
//    "b0000".U,
//    Array(
//      0.U -> ("b0001".U << byteOffset), // SB
//      1.U -> ("b0011".U << byteOffset), // SH
//      2.U -> "b1111".U                  // SW
//    )
//  )
//
//  // 构建写入数据
//  val writeData = Wire(UInt(32.W))
//  writeData := io.ex_mem.writeData << byteShift
//
//  // 读取指令
//  for (i <- 0 until instWidth) {
//    val fetchAddr = (io.if_mem.instAddr >> 2.U) + i.U
//    io.mem_id.inst(i) := Mux(io.reset, 0x13.U, memInside.read(fetchAddr))
//  }
//
//  // 读取数据
//  val readData = memInside.read((io.ex_mem.dataAddr >> 2.U))
//
//  // 根据func3处理不同类型的加载
//  io.mem_lsu.data := MuxLookup(
//    io.ex_mem.func3,
//    readData,
//    Array(
//      0.U -> Cat(Fill(24, readData(7+(byteShift.asUInt))), readData(7+(byteShift.asUInt), 0+byteShift)),      // LB
//      1.U -> Cat(Fill(16, readData(15+(byteShift.asUInt))), readData(15+(byteShift.asUInt), 0+byteShift)),    // LH
//      2.U -> readData,                                                                                         // LW
//      4.U -> Cat(0.U(24.W), readData(7+(byteShift.asUInt), 0+byteShift)),                                     // LBU
//      5.U -> Cat(0.U(16.W), readData(15+(byteShift.asUInt), 0+byteShift))                                     // LHU
//    )
//  )
//
//  // 写入内存
//  when(io.ex_mem.writeEn) {
//    val oldData = memInside.read(io.ex_mem.dataAddr >> 2.U)
//    val newData = (writeData & byteMask.asBools.map(b => Fill(8, b)).reduce(Cat(_, _))) |
//      (oldData & ~byteMask.asBools.map(b => Fill(8, b)).reduce(Cat(_, _)))
//    memInside.write(io.ex_mem.dataAddr >> 2.U, newData)
//  }
//}
