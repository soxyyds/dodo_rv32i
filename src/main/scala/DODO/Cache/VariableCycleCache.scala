package DODO.Cache

import DODO.{if_mem, lsu_mem_c, mem_id}
import chisel3._
import chisel3.util._

// ------------------------
// 参数定义
// ------------------------

// 数据访问大小枚举
object MemSize {
  val BYTE = 0.U(2.W)  // 8位
  val HALF = 1.U(2.W)  // 16位
  val WORD = 2.U(2.W)  // 32位
}

// ------------------------
// I/O Bundle 定义
// ------------------------

// 1. 底层双端口物理内存接口
// addrA/addrB: 访问地址
// dinA/dinB: 写入数据
// wenA/wenB: 写使能
// doutA/doutB: 读取数据
// 默认地址宽度64位，数据宽度32位
// 默认地址宽度64位，数据宽度32位
class DualPortMemIO extends Bundle {
  val addr  = Input(UInt(64.W))
  val wdata = Input(UInt(32.W))
  val wen   = Input(Bool())
  val ren   = Input(Bool())
  val rdata = Output(UInt(32.W))
}

class DCachePortIO extends Bundle {
  val addr    = Input(UInt(64.W))      // 地址
  val wdata   = Input(UInt(32.W))      // 写数据
  val wen     = Input(Bool())                 // 写使能
  val ren     = Input(Bool())                 // 读使能
  val rdata   = Output(UInt(32.W))     // 读数据
  val isHalf  = Input(Bool())                 // 是否半字（16位）
  val isByte  = Input(Bool())                 // 是否字节（8位）
  val isUnsigned = Input(Bool())              // 是否无符号
  val ready   = Output(Bool())                // 是否命中/可返回
}

class DCacheDualPortIO extends Bundle {
  val port1 = new DCachePortIO
  val port2 = new DCachePortIO
}

class Mem1IO extends Bundle {
  val valid    = Input(Bool())
  val addr     = Input(UInt(64.W))
  val wdata    = Input(UInt(32.W))
  val wen      = Input(Bool())
  val ren      = Input(Bool())
  val isHalf   = Input(Bool())
  val isByte   = Input(Bool())
  val isUnsigned = Input(Bool())
  val ready    = Output(Bool())
  val stall    = Output(Bool())
}

class Mem2IO extends Bundle {
  val rdata = Output(UInt(32.W))
  val valid = Output(Bool())
}

class MemCtrlIO extends Bundle {
  val addr1     = Input(UInt(64.W))
  val addr2     = Input(UInt(64.W))
  val valid1    = Input(Bool())
  val valid2    = Input(Bool())
  val op1Ld     = Input(Bool())
  val op2Ld     = Input(Bool())
  val op1St     = Input(Bool())
  val op2St     = Input(Bool())

  val kill1       = Output(Bool()) // 是否取消指令1（写冲突）
  val bypass2     = Output(Bool()) // 是否 bypass 指令2 load 的数据
  val stall2      = Output(Bool()) // 是否 stall 第二条指令
  val globalStall = Output(Bool())
}

class MEMIO extends Bundle {
  val req1  = Flipped(new Mem1IO)  // Mem1端口1
  val req2  = Flipped(new Mem1IO)  // Mem1端口2
  val resp1 = new Mem2IO                      // Mem2响应1
  val resp2 = new Mem2IO                      // Mem2响应2
}

// ------------------------
// 模块定义：冲突检测 MemCtrl
// ------------------------
// 功能：检测两条并发访存请求的地址和操作类型，生成 stall 与 bypass 控制信号
class MemCtrl extends Module {
  val io = IO(new MemCtrlIO)

  // 1. 判断两条请求地址是否相同且都有效
  val sameAddr = io.valid1 && io.valid2 && (io.addr1 === io.addr2)

  val bothLoad   = io.op1Ld && io.op2Ld
  val bothStore  = io.op1St && io.op2St
  val stThenLd   = io.op1St && io.op2Ld
  val ldThenSt   = io.op1Ld && io.op2St

  // 默认控制信号
  io.kill1       := false.B
  io.bypass2     := false.B
  io.stall2      := false.B
  io.globalStall := false.B

  when (sameAddr) {
    when (bothStore) {
      io.kill1 := true.B  // 两条store写相同地址 → kill掉第一条
    }.elsewhen (stThenLd) {
      io.bypass2 := true.B  // 第一条store写，第二条load读同地址 → bypass
    }.elsewhen (ldThenSt) {
      io.stall2 := true.B   // 第一条load读，第二条store写同地址 → stall 第二条
      io.globalStall := true.B
    }
  }
}

class DCache extends Module {
  val io = IO(new DCacheDualPortIO)

  val cacheSize = 64  // Cache 行数，可调
  val indexWidth = log2Ceil(cacheSize)

  // Tag 和 Valid Bit
  val tagArray = RegInit(VecInit(Seq.fill(cacheSize)(0.U(52.W))))   // 地址高52位作Tag（64-12）
  val validArray = RegInit(VecInit(Seq.fill(cacheSize)(false.B)))

  // Data Array
  val dataArray = RegInit(VecInit(Seq.fill(cacheSize)(0.U(32.W))))

  // Miss Wait Registers（访存调度器内嵌）
  val waitCounter1 = RegInit(0.U(2.W))
  val waiting1     = RegInit(false.B)
  val missAddr1    = Reg(UInt(64.W))
  val missTag1     = Reg(UInt(52.W))
  val missIdx1     = Reg(UInt(indexWidth.W))
  val missData1    = Reg(UInt(32.W))

  val waitCounter2 = RegInit(0.U(2.W))
  val waiting2     = RegInit(false.B)
  val missAddr2    = Reg(UInt(64.W))
  val missTag2     = Reg(UInt(52.W))
  val missIdx2     = Reg(UInt(indexWidth.W))
  val missData2    = Reg(UInt(32.W))

  // -------------------------------
  // Helper: load 数据裁剪与符号扩展
  def formatLoadData(data: UInt, isHalf: Bool, isByte: Bool, isUnsigned: Bool): UInt = {
    Mux(isByte,
      Mux(isUnsigned, data(7,0), Cat(Fill(24, data(7)), data(7,0))),
      Mux(isHalf,
        Mux(isUnsigned, data(15,0), Cat(Fill(16, data(15)), data(15,0))),
        data
      )
    )
  }

  // -------------------------------
  // 每个端口的访问处理
  def handlePort(port: DCachePortIO,
                 waitCounter: UInt,
                 waiting: Bool,
                 missAddr: UInt,
                 missTag: UInt,
                 missIdx: UInt,
                 missData: UInt): Unit = {

    val index = port.addr(indexWidth + 1, 2)
    val tag   = port.addr(63, indexWidth + 2)
    val cacheHit = validArray(index) && tagArray(index) === tag
    val cacheData = dataArray(index)

    when (port.ren || port.wen) {
      when (cacheHit) {
        // 命中
        port.ready := true.B

        when (port.ren) {
          port.rdata := formatLoadData(cacheData, port.isHalf, port.isByte, port.isUnsigned)
        }

        when (port.wen) {
          dataArray(index) := port.wdata
        }

      }.otherwise {
        // 未命中，触发等待
        port.ready := false.B
        when (!waiting) {
          missAddr := port.addr
          missTag := tag
          missIdx := index
          waiting := true.B
          waitCounter := 2.U  // 模拟2周期延迟
        }
      }
    }.otherwise {
      port.ready := false.B
    }

    // Miss 等待计数逻辑
    when (waiting) {
      when (waitCounter === 0.U) {
        // 模拟从主存加载
        tagArray(missIdx) := missTag
        dataArray(missIdx) := missData  // 可连接真实主存
        validArray(missIdx) := true.B
        waiting := false.B
      }.otherwise {
        waitCounter := waitCounter - 1.U
      }
    }
  }

  // 初始化输出
  io.port1.rdata := 0.U
  io.port2.rdata := 0.U
  io.port1.ready := false.B
  io.port2.ready := false.B

  // 简单模拟：假设主存总返回 0x12345678（实际可连真实 mem）
  missData1 := 0x12345678.U
  missData2 := 0x12345678.U

  // 分别处理两个端口
  handlePort(io.port1, waitCounter1, waiting1, missAddr1, missTag1, missIdx1, missData1)
  handlePort(io.port2, waitCounter2, waiting2, missAddr2, missTag2, missIdx2, missData2)
}



// ------------------------
// MEM顶层模块
// ------------------------
class MEM extends Module {
  val io = IO(new MEMIO)

  // 子模块实例化
  val dcache = Module(new DCache)
  val memCtrl = Module(new MemCtrl)

  // -------------------------------
  // 连接 MemCtrl 控制模块
  memCtrl.io.addr1  := io.req1.addr
  memCtrl.io.addr2  := io.req2.addr
  memCtrl.io.valid1 := io.req1.valid
  memCtrl.io.valid2 := io.req2.valid
  memCtrl.io.op1Ld  := io.req1.ren
  memCtrl.io.op2Ld  := io.req2.ren
  memCtrl.io.op1St  := io.req1.wen
  memCtrl.io.op2St  := io.req2.wen

  // -------------------------------
  // 控制逻辑处理
  val req1_fire = io.req1.valid
  val req2_fire = io.req2.valid && !memCtrl.io.stall2

  val kill1    = memCtrl.io.kill1
  val bypass2  = memCtrl.io.bypass2
  val stall2   = memCtrl.io.stall2

  // -------------------------------
  // 请求发送给 DCache
  dcache.io.port1.addr       := io.req1.addr
  dcache.io.port1.wdata      := io.req1.wdata
  dcache.io.port1.wen        := io.req1.wen && !kill1
  dcache.io.port1.ren        := io.req1.ren && !kill1
  dcache.io.port1.isHalf     := io.req1.isHalf
  dcache.io.port1.isByte     := io.req1.isByte
  dcache.io.port1.isUnsigned := io.req1.isUnsigned

  dcache.io.port2.addr       := io.req2.addr
  dcache.io.port2.wdata      := io.req2.wdata
  dcache.io.port2.wen        := io.req2.wen && req2_fire
  dcache.io.port2.ren        := io.req2.ren && req2_fire
  dcache.io.port2.isHalf     := io.req2.isHalf
  dcache.io.port2.isByte     := io.req2.isByte
  dcache.io.port2.isUnsigned := io.req2.isUnsigned

  // -------------------------------
  // 输出响应：Mem2IO
  io.resp1.rdata := dcache.io.port1.rdata
  io.resp1.valid := io.req1.valid && dcache.io.port1.ready

  // 对于第二条指令，如果要 bypass，就返回第一条写入的数据
  val bypassData = io.req1.wdata
  io.resp2.rdata := Mux(bypass2, bypassData, dcache.io.port2.rdata)
  io.resp2.valid := io.req2.valid && (dcache.io.port2.ready || bypass2)

  // -------------------------------
  // Stall 信息
  io.req1.stall := io.req1.valid && !dcache.io.port1.ready
  io.req2.stall := io.req2.valid && (!dcache.io.port2.ready && !bypass2 || stall2)
}


//class DualPortDRAMIO(memDepth: Int) extends Bundle {
//  val addrA  = Input(UInt(log2Ceil(memDepth).W))
//  val dinA   = Input(UInt(32.W))
//  val wenA   = Input(Bool())
//  val rdataA = Output(UInt(32.W))
//
//  val addrB  = Input(UInt(log2Ceil(memDepth).W))
//  val dinB   = Input(UInt(32.W))
//  val wenB   = Input(Bool())
//  val rdataB = Output(UInt(32.W))
//
//  // 读使能可根据需求额外添加
//}
//
//
//
//class DualPortDRAM(memDepth: Int) extends Module {
//  val io = IO(new DualPortDRAMIO(memDepth))
//
//  // 使用 SyncReadMem 存储 32 位数据
//  val mem = SyncReadMem(memDepth, UInt(32.W))
//
//  // 端口A读写
//  val portAread = mem.read(io.addrA) // 同步读
//  val portAdata = Mux(io.wenA, io.dinA, portAread)
//  when(io.wenA){
//    mem.write(io.addrA, io.dinA)
//  }
//  io.rdataA := portAread
//
//  // 端口B读写
//  val portBread = mem.read(io.addrB)
//  val portBdata = Mux(io.wenB, io.dinB, portBread)
//  when(io.wenB){
//    mem.write(io.addrB, io.dinB)
//  }
//  io.rdataB := portBread
//}
