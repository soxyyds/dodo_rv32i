package DODO

import chisel3._
import chisel3.util._

class DCache(cacheSize: Int = 4 * 1024) extends Module {
  val io = IO(new Bundle {
    val cpu = Flipped(new DCacheIO)
    val mem = new MemIO
    val stall = Output(Bool()) // 向流水线发送停顿信号
  })

  // 缓存参数
  val blockSize = 4 // 4字节/块
  val numSets = cacheSize / (blockSize * 2) // 二路组相联
  val offsetBits = log2Ceil(blockSize)
  val indexBits = log2Ceil(numSets)
  val tagBits = 32 - indexBits - offsetBits

  // 缓存状态
  val idle :: compare :: update :: Nil = Enum(3)
  val state = RegInit(idle)

  // 缓存结构
  class CacheLine extends Bundle {
    val valid = Bool()
    val tag = UInt(tagBits.W)
    val data = UInt(32.W)
  }

  val cache = Seq.fill(2)(SyncReadMem(numSets, new CacheLine))
  val fifoPtr = RegInit(VecInit(Seq.fill(numSets)(0.U(1.W)))) // FIFO替换指针

  // 地址解析
  val reqAddr = io.cpu.req.bits.addr
  val reqTag = reqAddr(31, indexBits + offsetBits)
  val reqIndex = reqAddr(indexBits + offsetBits - 1, offsetBits)

  // 请求寄存器
  val reqReg = Reg(new DCacheReq)
  val reqValid = RegInit(false.B)

  // 缓存命中逻辑
  val way0Data = cache(0).read(reqIndex)
  val way1Data = cache(1).read(reqIndex)

  val hit0 = way0Data.valid && way0Data.tag === reqTag
  val hit1 = way1Data.valid && way1Data.tag === reqTag
  val hit = hit0 || hit1
  val hitData = Mux(hit0, way0Data.data, way1Data.data)
  val hitWay = Mux(hit0, 0.U, 1.U)

  // 内存请求信号 - 关键修改点
  io.mem.req_valid := state === compare && !hit
  io.mem.req_bits.addr := reqReg.addr
  io.mem.req_bits.data := reqReg.data
  io.mem.req_bits.mask := reqReg.mask
  io.mem.req_bits.cmd := reqReg.cmd

  // 默认值
  io.cpu.req.ready := state === idle
  io.cpu.resp.valid := false.B
  io.cpu.resp.bits.data := 0.U
  io.mem.resp_ready := state === update
  io.stall := state =/= idle

  switch(state) {
    is(idle) {
      when(io.cpu.req.valid && !io.cpu.stall) {
        reqReg := io.cpu.req.bits
        reqValid := true.B
        state := compare
      }
    }

    is(compare) {
      when(hit) {
        // 缓存命中，1周期返回
        io.cpu.resp.valid := true.B
        io.cpu.resp.bits.data := hitData

        // 处理写请求
        when(reqReg.cmd === DCacheReqCmd.write) {
          val newLine = Wire(new CacheLine)
          newLine.valid := true.B
          newLine.tag := reqTag
          newLine.data := reqReg.data

          when(hit0) {
            cache(0).write(reqIndex, newLine)
          }.otherwise {
            cache(1).write(reqIndex, newLine)
          }
        }

        // 重置状态
        reqValid := false.B
        state := idle
      }.elsewhen(io.mem.req_ready) {
        // 缓存未命中且内存已接收请求，直接进入更新状态
        state := update
      }
    }

    is(update) {
      when(io.mem.resp_valid) {
        // 更新缓存并向CPU提供数据
        val replacementWay = fifoPtr(reqIndex)

        val newLine = Wire(new CacheLine)
        newLine.valid := true.B
        newLine.tag := reqTag
        newLine.data := io.mem.resp_bits.data

        when(replacementWay === 0.U) {
          cache(0).write(reqIndex, newLine)
        }.otherwise {
          cache(1).write(reqIndex, newLine)
        }

        // 更新FIFO指针
        fifoPtr(reqIndex) := ~fifoPtr(reqIndex)

        // 返回数据给CPU
        io.cpu.resp.valid := true.B
        io.cpu.resp.bits.data := Mux(
          reqReg.cmd === DCacheReqCmd.read,
          io.mem.resp_bits.data,
          reqReg.data
        )

        // 重置状态
        reqValid := false.B
        state := idle
      }
    }
  }
}

class DCacheReq extends Bundle {
  val addr = UInt(32.W)
  val data = UInt(32.W)
  val mask = UInt(4.W)
  val cmd = UInt(1.W)  // 0=读，1=写
}

object DCacheReqCmd {
  val read = 0.U(1.W)
  val write = 1.U(1.W)
}

class DCacheResp extends Bundle {
  val data = UInt(32.W)
}

class DCacheIO extends Bundle {
  val req = Decoupled(new DCacheReq)
  val resp = Flipped(Decoupled(new DCacheResp))
  val stall = Input(Bool())
}

class MemReq extends Bundle {
  val addr = UInt(64.W)    // 64位地址
  val data = UInt(32.W)    // 32位数据
  val mask = UInt(4.W)     // 4字节掩码
  val cmd = UInt(1.W)      // 0=读，1=写
}

class MemResp extends Bundle {
  val data = UInt(32.W)    // 32位响应数据
}

class MemIO extends Bundle {
  // 请求通道
  val req_valid = Output(Bool())       // 请求有效信号
  val req_ready = Input(Bool())        // 请求就绪信号
  val req_bits = Output(new MemReq)    // 请求数据

  // 响应通道
  val resp_valid = Input(Bool())       // 响应有效信号
  val resp_ready = Output(Bool())      // 响应就绪信号
  val resp_bits = Input(new MemResp)   // 响应数据
}

class MainMemory(memSize: Int = 1024 * 1024, queueDepth: Int = 4) extends Module {
  val io = IO(new Bundle {
    val mem_port1 = Flipped(new MemIO)  // 第一个端口（如指令获取）
    val mem_port2 = Flipped(new MemIO)  // 第二个端口（如数据访问）
  })

  // 内存实现（仍为单端口）
  val mem = Mem(memSize, UInt(32.W))

  // 请求队列 - 缓存两个端口的请求
  val pendingQueue = Module(new Queue(new MemReqWithSource(), queueDepth))

  // 扩展请求类，记录请求来源
  class MemReqWithSource extends Bundle {
    val req = new MemReq()
    val source = Bool()  // false = port1, true = port2
  }

  // 状态机
  val idle :: write :: Nil = Enum(2)  // 优化：移除read状态，使读取更快
  val state = RegInit(idle)

  // 追踪当前处理的请求来源
  val currentSource = RegInit(false.B)

  // 优先级处理：port1 > port2
  val port1Ready = pendingQueue.io.enq.ready
  val port2Ready = pendingQueue.io.enq.ready && !io.mem_port1.req_valid

  io.mem_port1.req_ready := port1Ready
  io.mem_port2.req_ready := port2Ready

  // 请求入队逻辑
  when(io.mem_port1.req_valid && port1Ready) {
    val reqWithSource = Wire(new MemReqWithSource())
    reqWithSource.req := io.mem_port1.req_bits
    reqWithSource.source := false.B
    pendingQueue.io.enq.valid := true.B
    pendingQueue.io.enq.bits := reqWithSource
  }.elsewhen(io.mem_port2.req_valid && port2Ready) {
    val reqWithSource = Wire(new MemReqWithSource())
    reqWithSource.req := io.mem_port2.req_bits
    reqWithSource.source := true.B
    pendingQueue.io.enq.valid := true.B
    pendingQueue.io.enq.bits := reqWithSource
  }.otherwise {
    pendingQueue.io.enq.valid := false.B
  }

  // 处理寄存器
  val addr_reg = RegInit(0.U(64.W))
  val data_reg = RegInit(0.U(32.W))
  val mask_reg = RegInit(0.U(4.W))

  // 默认响应无效
  io.mem_port1.resp_valid := false.B
  io.mem_port2.resp_valid := false.B
  io.mem_port1.resp_bits.data := 0.U
  io.mem_port2.resp_bits.data := 0.U

  // 状态机处理
  switch(state) {
    is(idle) {
      when(pendingQueue.io.deq.valid) {
        val request = pendingQueue.io.deq.bits
        pendingQueue.io.deq.ready := true.B

        when(request.req.cmd === 0.U) { // 读操作
          // 优化：直接读取内存并在同一周期内响应
          val readData = mem.read(request.req.addr(log2Ceil(memSize)-1, 0))

          when(!request.source) {
            // 发送到port1
            io.mem_port1.resp_valid := true.B
            io.mem_port1.resp_bits.data := readData
            when(!io.mem_port1.resp_ready) {
              // 只有在接收端未准备好时才保存数据，否则直接完成
              addr_reg := request.req.addr
              data_reg := readData
              mask_reg := request.req.mask
              currentSource := request.source
              state := idle // 保持空闲状态，下个周期重试
            }
          }.otherwise {
            // 发送到port2
            io.mem_port2.resp_valid := true.B
            io.mem_port2.resp_bits.data := readData
            when(!io.mem_port2.resp_ready) {
              addr_reg := request.req.addr
              data_reg := readData
              mask_reg := request.req.mask
              currentSource := request.source
              state := idle // 保持空闲状态，下个周期重试
            }
          }
        }.otherwise { // 写操作
          addr_reg := request.req.addr
          data_reg := request.req.data
          mask_reg := request.req.mask
          currentSource := request.source
          state := write
        }
      }.otherwise {
        pendingQueue.io.deq.ready := false.B
      }
    }

    is(write) {
      val addr = addr_reg(log2Ceil(memSize)-1, 0)
      val old_data = mem.read(addr)
      val byte_mask = VecInit(mask_reg.asBools.map(Fill(8, _))).asUInt
      val new_data = (data_reg & byte_mask) | (old_data & (~byte_mask).asUInt)
      mem.write(addr, new_data)

      // 写完成后立即响应
      when(!currentSource) {
        io.mem_port1.resp_valid := true.B
        io.mem_port1.resp_bits.data := data_reg
        when(io.mem_port1.resp_ready) {
          state := idle
        }
      }.otherwise {
        io.mem_port2.resp_valid := true.B
        io.mem_port2.resp_bits.data := data_reg
        when(io.mem_port2.resp_ready) {
          state := idle
        }
      }
    }
  }
}