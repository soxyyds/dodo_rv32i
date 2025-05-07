package DODO

import chisel3._
import chisel3.util._

class CSRRegisterBank extends Module {
  val io = IO(new Bundle {
    val csr_wen = Input(Bool())
    val csr_addr = Input(UInt(12.W))
    val csr_wdata = Input(UInt(64.W))
    val csr_rdata = Output(UInt(64.W))
    val mcycle = Output(UInt(64.W))
  })

  val mcycle = RegInit(0.U(64.W))

  // mcycle处理逻辑
  when(io.csr_wen && io.csr_addr === 0xB00.U) {
    mcycle := io.csr_wdata     // 写入时更新值
  }.otherwise {
    mcycle := mcycle + 1.U     // 否则每个周期自增
  }

  io.mcycle := mcycle
  io.csr_rdata := Mux(io.csr_addr === 0xB00.U, mcycle, 0.U)
}
