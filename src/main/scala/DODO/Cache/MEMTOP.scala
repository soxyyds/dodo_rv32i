package DODO.Cache

import chisel3._
import chisel3.util._
import DODO.Cache.{MEM, DualPortDRAM, DualPortDRAMIO}

class TopLevelMemSystem(memDepth: Int) extends Module {
  val io = IO(new Bundle {
    val memReq1  = Flipped(new MEMIO)
    val memReq2  = Flipped(new MEMIO)
    val memResp1 = new Mem2IO
    val memResp2 = new Mem2IO
  })

  // 实例化不定周期访存模块
  val memSystem = Module(new MEM)
  // 实例化真实存储DRAM
  val dram      = Module(new DualPortDRAM(memDepth))

  // 连接MEM端口到IO
  memSystem.io.req1 <> io.memReq1
  memSystem.io.req2 <> io.memReq2
  io.memResp1 <> memSystem.io.resp1
  io.memResp2 <> memSystem.io.resp2

  // 这里示例将MEM中的未命中读取替换为DRAM
  // 具体逻辑可在DCache里添加端口，将miss地址与写数据送到DRAM
  // dram.io.addrA := ...
  // dram.io.wenA  := ...
  // dram.io.dinA  := ...
  // memSystem的dcache在等待完成后取dram.io.rdataA完成写回
}