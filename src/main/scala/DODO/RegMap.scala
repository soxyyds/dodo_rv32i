//RegMap.scala
package DODO

import chisel3._
import chisel3.util._

class RegMap extends Module{
  val io = IO(new Bundle{
    val IDRMA = Input(new InstCtrlBlock)
    val IDRMB = Input(new InstCtrlBlock)
    val RMDPA = Output(new InstCtrlBlock)
    val RMDPB = Output(new InstCtrlBlock)

    val FetchBlock = Input(Bool())
    val Rollback = Input(Bool())

    val CmtA = Input(new InstCtrlBlock)
    val CmtB = Input(new InstCtrlBlock)

    val FinA = Input(new InstCtrlBlock)
    val FinB = Input(new InstCtrlBlock)
    val FinC = Input(new InstCtrlBlock)
    val FinD = Input(new InstCtrlBlock)
    val FinE = Input(new InstCtrlBlock)

    val ReOrderNumA = Input(UInt(6.W))
    val ReOrderNumB = Input(UInt(6.W))
    val PhyRegStates = Output(UInt(128.W))
    val ArchRegValues = Output(Vec(32, UInt(64.W)))

  })

  val RegA = RegEnable(io.IDRMA, !io.FetchBlock)
  val RegB = RegEnable(io.IDRMB, !io.FetchBlock)
  val INSTA = Mux(!io.FetchBlock, RegA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  val INSTB = Mux(!io.FetchBlock, RegB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))

  val MapTable = new InitIncreaseRegBank(32, 7)
  // 稳定的映射关系，分支预测失误或中断处理时，可以一拍回滚
  val CmtTable = new InitIncreaseRegBank(32, 7)

  val ArchRegTable = new AbstractRegBank(32, 64)
  val phyRegStatesTable = new PhyRegStatesTable

  // 获取最新分配的物理寄存器号
  val pregdesA = Mux(INSTA.regdes === 0.U, 0.U, phyRegStatesTable.FreePhyRegisterA)
  val pregdesB = Mux(INSTB.regdes === 0.U, 0.U, phyRegStatesTable.FreePhyRegisterB)

  // 带走旧值，Cmt时候再来释放，B指令需要碰撞检测
  val cmtdesA = MapTable.read(INSTA.regdes)
  val cmtdesB = Mux(INSTA.regdes === INSTB.regdes, pregdesA, MapTable.read(INSTB.regdes))

  val pregsrc1 = MapTable.read(INSTA.regsrc1)
  val pregsrc2 = MapTable.read(INSTA.regsrc2)
  val pregsrc3 = Mux(INSTB.regsrc1 === INSTA.regdes, pregdesA, MapTable.read(INSTB.regsrc1))
  val pregsrc4 = Mux(INSTB.regsrc2 === INSTA.regdes, pregdesA, MapTable.read(INSTB.regsrc2))

  io.ArchRegValues := ArchRegTable.gpr
  io.PhyRegStates := phyRegStatesTable.AvailableList

  // Cmt处理，不受rollback影响
  when(io.CmtA.regdes =/= io.CmtB.regdes){
    CmtTable.write(io.CmtA.Valid, io.CmtA.regdes, io.CmtA.pregdes)
    ArchRegTable.write(io.CmtA.Valid, io.CmtA.regdes, io.CmtA.wbdata)
  }
  CmtTable.write(io.CmtB.Valid, io.CmtB.regdes, io.CmtB.pregdes)
  ArchRegTable.write(io.CmtB.Valid, io.CmtB.regdes, io.CmtB.wbdata)
  //to Assigned
  phyRegStatesTable.write(io.CmtA.Valid && io.CmtA.finish, io.CmtA.pregdes, 3.U(2.W))
  phyRegStatesTable.write(io.CmtB.Valid && io.CmtB.finish, io.CmtB.pregdes, 3.U(2.W))
  //to Free
  phyRegStatesTable.write(io.CmtA.Valid && io.CmtA.finish, io.CmtA.cmtdes, 0.U(2.W))
  phyRegStatesTable.write(io.CmtB.Valid && io.CmtB.finish, io.CmtB.cmtdes, 0.U(2.W))

  when(io.Rollback){
    // 预取指令的重命名取消
    // MapTable.table := CmtTable.table
    // jal/jalr指令，本身要写入，也要rollback
    // 逻辑上需要先写入CmtTable，再回滚到MapTable
    // 为了在一拍完成，处理逻辑如下：
    for(i <- 0 to 31){
      MapTable.table(i) := Mux(i.U === io.CmtA.regdes, io.CmtA.pregdes, CmtTable.table(i))
    }

    // 物理寄存器状态的回滚
    // jal/jalr指令，本身要提交和释放，也要rollback
    // 逻辑上需要先free一个，assign一个，再回滚
    // 所以要提供两个特殊的物理寄存器号，第一个free，第二个assign
    phyRegStatesTable.Rollback(io.CmtA.cmtdes, io.CmtA.pregdes)

    io.RMDPA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.RMDPB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    RegA := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    RegB := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    // 写入最新的映射关系
    when(INSTA.regdes =/= INSTB.regdes) {
      MapTable.write(INSTA.Valid, INSTA.regdes, pregdesA)
    }
    MapTable.write(INSTB.Valid, INSTB.regdes, pregdesB)

    //to Mapped
    phyRegStatesTable.write(pregdesA =/= 0.U, pregdesA, 1.U(2.W))
    phyRegStatesTable.write(pregdesB =/= 0.U, pregdesB, 1.U(2.W))
    //to Executed
    phyRegStatesTable.write(io.FinA.Valid && io.FinA.finish, io.FinA.pregdes, 2.U(2.W))
    phyRegStatesTable.write(io.FinB.Valid && io.FinB.finish, io.FinB.pregdes, 2.U(2.W))
    phyRegStatesTable.write(io.FinC.Valid && io.FinC.finish, io.FinC.pregdes, 2.U(2.W))
    phyRegStatesTable.write(io.FinD.Valid && io.FinD.finish, io.FinD.pregdes, 2.U(2.W))
    phyRegStatesTable.write(io.FinE.Valid && io.FinE.finish, io.FinE.pregdes, 2.U(2.W))

    io.RMDPA := GenICB(pregsrc1, pregsrc2, pregdesA, cmtdesA, io.ReOrderNumA, INSTA)
    io.RMDPB := GenICB(pregsrc3, pregsrc4, pregdesB, cmtdesB, io.ReOrderNumB, INSTB)
  }

  def GenICB(pregsrc1: UInt, pregsrc2: UInt, pregdes: UInt, cmtdes: UInt, reOrderNum: UInt, IDRM: InstCtrlBlock): InstCtrlBlock = {
    val ICB = Wire(new InstCtrlBlock)
    ICB.Valid := IDRM.Valid
    ICB.inst := IDRM.inst
    ICB.pc := IDRM.pc
    ICB.isa := IDRM.isa
    ICB.finish := IDRM.finish
    ICB.reOrderNum := reOrderNum
    ICB.regdes := IDRM.regdes
    ICB.regsrc1 := IDRM.regsrc1
    ICB.regsrc2 := IDRM.regsrc2
    ICB.pregsrc1 := pregsrc1
    ICB.pregsrc2 := pregsrc2
    ICB.pregdes := pregdes
    ICB.cmtdes := cmtdes
    ICB.src1 := IDRM.src1
    ICB.src2 := IDRM.src2
    ICB.imm := IDRM.imm
    ICB.wbdata := IDRM.wbdata
    ICB.jump := IDRM.jump
    ICB.branch := IDRM.branch
    ICB.load := IDRM.load
    ICB.store := IDRM.store
    ICB.bppredIndex := IDRM.bppredIndex
    ICB.bpPredTaken := IDRM.bpPredTaken
    ICB.bpPredTarget := IDRM.bpPredTarget
    ICB.csr_addr := IDRM.csr_addr
    ICB.csr_wdata := IDRM.csr_wdata
    ICB
  }
}

class AbstractRegBank(depth: Int, width: Int) {
  val gpr = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))
  def read(addr: UInt): UInt = { gpr(addr) }
  def write(wen: Bool, addr: UInt, data: UInt): Unit = {
    when(wen && addr =/= 0.U){ gpr(addr) := data }
  }
}

class InitIncreaseRegBank(depth: Int, width: Int) {
  val IncSeq = VecInit(0.U(7.W),1.U(7.W),2.U(7.W),3.U(7.W),4.U(7.W),5.U(7.W),6.U(7.W),7.U(7.W),8.U(7.W),9.U(7.W),10.U(7.W),11.U(7.W),12.U(7.W),13.U(7.W),14.U(7.W),15.U(7.W),16.U(7.W),17.U(7.W),18.U(7.W),19.U(7.W),20.U(7.W),21.U(7.W),22.U(7.W),23.U(7.W),24.U(7.W),25.U(7.W),26.U(7.W),27.U(7.W),28.U(7.W),29.U(7.W),30.U(7.W),31.U(7.W))
  val table = RegInit(IncSeq)
  def read(addr: UInt): UInt = { table(addr) }
  def write(wen: Bool, addr: UInt, data: UInt): Unit = {
    when(wen && addr =/= 0.U){ table(addr) := data }
  }
}


class PhyRegStatesTable {
  // Free: 00
  // Mapped: 01
  // Executed: 10
  // Assigned: 11

  // 复位时，低位32个是Assigned，高位96个是Free
  val SeqAssigned = Seq.fill(32)(3.U(2.W))
  val SeqFree = Seq.fill(96)(0.U(2.W))
  val SeqInit = SeqAssigned ++ SeqFree
  val PhyRegStates = RegInit(VecInit(SeqInit))

  // 复位时，0x1111_1111_1111_1111_1111_1111_0000_0000
  val FreeList = GenFreeList()
  val NewFreeList = FreeList - lowbit(FreeList)
  // 找到FreeList中最低一个1的下标，FreeList(0)恒为0，所以没有0就返回0
  def FreePhyRegisterA(): UInt = { Log2(lowbit(FreeList)) }
  // 找到FreeList中次低一个1的下标，同样没有0返回0
  def FreePhyRegisterB(): UInt = { Log2(lowbit(NewFreeList)) }

  // 给保留站广播
  def AvailableList(): UInt = {
    val availablelist = Wire(Vec(128, UInt(1.W)))
    for(i <- 0 to 127){
      availablelist(i) := PhyRegStates(i)(1)
    }
    availablelist.asUInt
  }

  def write(wen: Bool, addr: UInt, data: UInt): Unit = {
    when(wen && addr =/= 0.U){ PhyRegStates(addr) := data }
  }

  // 物理寄存器状态的回滚
  // jal/jalr指令，本身要提交和释放，也要rollback
  // 逻辑上需要先free一个，assign一个，再回滚
  // 所以要提供两个特殊的物理寄存器号，第一个free，第二个assign
  def Rollback(free: UInt, assign: UInt): Unit = {
    for(i <- 1 to 127){
      when(i.U === free){
        PhyRegStates(i) := 0.U
      }.elsewhen(i.U === assign){
        PhyRegStates(i) := 3.U
      }.otherwise{
        PhyRegStates(i) := Mux(PhyRegStates(i) === 3.U, 3.U, 0.U)
      }
    }
  }

  def GenFreeList(): UInt = {
    val freelist = Wire(Vec(128, UInt(1.W)))
    for(i <- 0 to 127){
      freelist(i) := PhyRegStates(i) === 0.U
    }
    freelist.asUInt
  }
}

//input : 00101010110
//output: 00000000010

//input : 00101010110
//output: 00100000000
object highbit {
  def apply(data: UInt): UInt = {
    Reverse(lowbit(Reverse(data)))
  }
}