package DODO

import chisel3._
import chisel3.util._


class Memory extends Module {
  val io = IO(new Bundle {
    val EXMEM = Input(new InstCtrlBlock)
    val FinE = Output(new InstCtrlBlock)
    val CmtA = Input(new InstCtrlBlock) //内存指令提交了之后才能将相应的值写入内存
    val mem_inst = Output(new InstCtrlBlock) //内存指令的输出
    val mem_Valid = Output(Bool()) //内存指令的有效性
    val ForwardLoad = Output(new LoadIssue)
    val ForwardStore = Input(new StoreIssue)
    val d_data = Output(UInt(64.W)) //从内存中读取的数据
    val read_func3 = Output(UInt(3.W)) //读取的功能码
    val Rollback = Input(Bool())
    val DataRam = new RAMHelperIO_2 //相当于多了定义类的input和output
  })

  // === 1. 时序寄存当前指令 ===
  val INST = RegNext(io.EXMEM)
  io.ForwardLoad := Mux(INST.isa.Lclass, io.FinE.load, WireInit(0.U.asTypeOf(new LoadIssue())))
  // === 2. RAM 接口地址转换 ===
  //其实我们不用管mem模块里面到底有多少的信号和输出端口 我们只要关注好我们这个哈佛架构的管道到底要多少信号
  io.DataRam.read_address := Mux(INST.load.Valid, INST.load.addr, 0.U(64.W)) //如果是load指令就用load的地址
  io.DataRam.data_wen := (io.CmtA.Valid && io.CmtA.store.Valid)
  io.DataRam.data_address := Mux(io.CmtA.store.Valid, io.CmtA.store.addr, 0.U(64.W)) //如果是store指令就用store的地址，
  io.DataRam.data_wdata := io.CmtA.store.data //获取要存入里面的数据
  io.DataRam.func3_write := io.CmtA.store.mask //获取掩码 mask掩码要修改，这个mask是在execute里面生成的
  io.DataRam.func3_read := 2.U //获取load的掩码
  io.mem_Valid := (io.CmtA.Valid && (io.CmtA.store.Valid || io.CmtA.load.Valid))
  io.mem_inst := Mux(io.mem_Valid, io.CmtA, WireInit(0.U.asTypeOf(new InstCtrlBlock)))
  // === 3. Store Forwarding 拼接逻辑 ===
  val wdata = Mux(io.ForwardStore.Valid, io.ForwardStore.data, 0.U(64.W))
  val processedwdata = WireInit(0.U(64.W)) // Store Forwarding 的数据
  // Store Forwarding 数据生成逻辑
  when(io.ForwardStore.Valid) {
    // 根据不同的加载类型生成对应的数据
    switch(io.ForwardStore.mask) {
      is(0.U) { // SB - 字节存储，拼接成64位数据
        processedwdata := Cat(0.U(32.W), io.ForwardStore.data(7,0),io.ForwardStore.data(7,0),io.ForwardStore.data(7,0),io.ForwardStore.data(7,0))
      }
      is(1.U) { // SH - 半字存储，拼接成64位数据
        processedwdata := Cat(0.U(32.W), io.ForwardStore.data(15, 0), io.ForwardStore.data(15, 0))
      }
      is(2.U) { // SW - 字存储，直接使用64位数据
        processedwdata := Cat(0.U(32.W), io.ForwardStore.data(31, 0)) // LW - 字加载，直接使用32位数据
      }
    }
  }.otherwise {   // 如果没有 Store Forwarding，则使用默认值
    processedwdata := Cat(0.U(32.W), io.ForwardStore.data(31, 0))
  }


  // val wmask = Mux(io.ForwardStore.Valid, io.ForwardStore.mask, 0.U(64.W))
  io.read_func3 := io.ForwardStore.mask
  val wmask = WireInit(0.U(64.W)) // Store Forwarding 的掩码
  // Store Forwarding 掩码生成逻辑
  when(io.ForwardStore.Valid) {
    // 根据不同的加载类型生成对应的掩码
    switch(io.ForwardStore.mask) {
      is(0.U) { // LB - 字节加载，掩码为0x000000FF
        switch(io.ForwardStore.addr(1,0)) {
          is(0.U) { wmask := 0x000000FFL.U(64.W) } // 低字节
          is(1.U) { wmask := 0x0000FF00L.U(64.W) } // 次低字节
          is(2.U) { wmask := 0x00FF0000L.U(64.W) } // 次高字节
          is(3.U) { wmask := 0xFF000000L.U(64.W) } // 高字节
        }

      }
      is(1.U) { // LH - 半字加载，掩码为0x0000FFFF
        switch(io.ForwardStore.addr(1,0)) {
          is(0.U) { wmask := 0x0000FFFFL.U(64.W) } // 低半字
          is(1.U) { wmask := 0xFFFF0000L.U(64.W) } // 高半字
        }
      }
      is(2.U) { // LW - 字加载，掩码为0xFFFFFFFF
        wmask := 0xFFFFFFFFL.U(64.W) // 整个字
      }
    }
  }.otherwise {
    wmask := 0.U(64.W)
  }

  val d_data = (io.DataRam.data_rdata & (~wmask).asUInt) | (processedwdata & wmask)
  io.d_data := d_data // 将读取的数据输出到 d_data

  // === 4. 加载类型拼接处理 ===
  // 从64位总数据中逐级选择目标字节（load类型决定需要哪一段）
  val w_data = d_data(31,0)        // 选择 word（4 字节）
  val h_data = Mux(INST.load.addr(1).asBool, d_data(31,16), d_data(15,0)) // 选择 half-word（2 字节）
  val b_data = Mux(INST.load.addr(0).asBool, h_data(15,8), h_data(7,0)) // 选择 byte（1 字节）


//  val LD_data  = Mux(INST.isa.LD, d_data, 0.U)
  val LW_data  = Mux(INST.isa.LW, SignExt(w_data, 64), 0.U)
  val LH_data  = Mux(INST.isa.LH, SignExt(h_data, 64), 0.U)
  val LB_data  = Mux(INST.isa.LB, SignExt(b_data, 64), 0.U)
//  val LWU_data = Mux(INST.isa.LWU, ZeroExt(w_data, 64), 0.U)
  val LHU_data = Mux(INST.isa.LHU, ZeroExt(h_data, 64), 0.U)
  val LBU_data = Mux(INST.isa.LBU, ZeroExt(b_data, 64), 0.U)

  // 汇总最终读取数据
  val LoadData =  LW_data | LH_data | LB_data |  LHU_data | LBU_data

  // === 5. ForwardLoad 直接输出 ===

  // === 6. 回滚处理 ===
  when(io.Rollback) {
    io.FinE := 0.U.asTypeOf(new InstCtrlBlock)
  } .otherwise {
    io.FinE := GenFin(INST.isa.Lclass || INST.isa.Sclass, LoadData, INST)
  }

  // === 7. FinE 构造函数 ===
  def GenFin(finish: Bool, LoadData: UInt, EXMEM: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB := EXMEM
    ICB.finish := finish
    ICB.wbdata := LoadData

    ICB.load.Valid := EXMEM.load.Valid
    ICB.load.addr  := EXMEM.load.addr
    ICB.load.data  := LoadData
    ICB.load.Ready := EXMEM.load.Ready

    ICB
}

}