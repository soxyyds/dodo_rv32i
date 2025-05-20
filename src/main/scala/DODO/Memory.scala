package DODO

import chisel3._
import chisel3.util._

// ------------------ 定义所需Bundle ------------------
//!!problem!!:need the definition of InstCtrlBlock(supposed to be in InstCtrlBlock)
//class InstCtrlBlock extends Bundle{
//  //base
//  val Valid = Bool()
//  val inst = UInt(32.W)
//  val pc = UInt(64.W)
//  val isa = new ISA
//  //reorder
//  val finish = Bool()
//  val reOrderNum = UInt(6.W)
//  //regs
//  val regdes = UInt(5.W)
//  val regsrc1 = UInt(5.W)
//  val regsrc2 = UInt(5.W)
//  //preg
//  val pregsrc1 = UInt(7.W)
//  val pregsrc2 = UInt(7.W)
//  val pregdes = UInt(7.W)
//  val cmtdes = UInt(7.W)
//  //exe
//  val src1 = UInt(64.W)
//  val src2 = UInt(64.W)
//  val imm = new IMM
//  val wbdata = UInt(64.W)
//  //bondle
//  val jump = new JumpIssue
//  val branch = new BranchIssue
//  val load = new LoadIssue
//  val store = new StoreIssue
//}

//class RAMHelperIO extends Bundle{
//  val clk = Output(Clock())
//  val en = Output(Bool())
//  val rIdx = Output(UInt(64.W))
//  val rdata = Input(UInt(64.W))
//  val wIdx = Output(UInt(64.W))
//  val wdata = Output(UInt(64.W))
//  val wmask = Output(UInt(64.W))
//  val wen = Output(Bool())
//}
// ------------------object define-----------------------
////extention
//object SignExt{
//  def apply(data: UInt, len: Int) = {
//    val aLen = data.getWidth
//    val signBit = data(aLen-1)
//    if (aLen >= len) data(len-1,0) else Cat(Fill(len - aLen, signBit), data)
//  }
//}
//
//object ZeroExt{
//  def apply(data: UInt, len: Int) = {
//    val aLen = data.getWidth
//    if (aLen >= len) data(len-1,0) else Cat(0.U((len - aLen).W), data)
//  }
//}

// ------------------ Memory 阶段模块实现 ------------------

class Memory extends Module {
  val io = IO(new Bundle {
    val EXMEM = Input(new InstCtrlBlock)
    val FinE = Output(new InstCtrlBlock)
    val CmtA = Input(new InstCtrlBlock)//内存指令提交了之后才能将相应的值写入内存

    val ForwardLoad = Output(new LoadIssue)
    val ForwardStore = Input(new StoreIssue)

    val Rollback = Input(Bool())
    val DataRam = new RAMHelperIO_2//相当于多了定义类的input和output
  })

  // === 1. 时序寄存当前指令 ===
  val INST = RegNext(io.EXMEM)

  // === 2. RAM 接口地址转换 ===
  //其实我们不用管mem模块里面到底有多少的信号和输出端口 我们只要关注好我们这个哈佛架构的管道到底要多少信号
  io.DataRam.data_wen  := (io.CmtA.Valid && io.CmtA.store.Valid)
  io.DataRam.data_address := INST.load.addr
  io.DataRam.data_wdata := io.CmtA.store.data //获取要存入里面的数据
  io.DataRam.func3 := io.CmtA.store.mask//获取掩码 mask掩码要修改，这个mask是在execute里面生成的

  // === 3. Store Forwarding 拼接逻辑 ===
  val wdata = Mux(io.ForwardStore.Valid, io.ForwardStore.data, 0.U(64.W))
  val wmask = Mux(io.ForwardStore.Valid, io.ForwardStore.mask, 0.U(64.W))
  val d_data = (io.DataRam.data_rdata & !wmask) | (wdata & wmask)

  // === 4. 加载类型拼接处理 ===
  // 从64位总数据中逐级选择目标字节（load类型决定需要哪一段）
  val w_data = Mux(INST.load.addr(2), d_data(63,32), d_data(31,0))        // 选择 word（4 字节）
  val h_data = Mux(INST.load.addr(1), w_data(31,16), w_data(15,0))        // 选择 half（2 字节）
  val b_data = Mux(INST.load.addr(0), h_data(15,8), h_data(7,0))          // 选择 byte（1 字节）



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
  io.ForwardLoad := {
    val fl = Wire(new LoadIssue)
    fl.Valid := INST.isa.Lclass
    fl.addr  := INST.load.addr
    fl.data  := LoadData
    fl.Ready := INST.load.Ready
    fl
  }

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