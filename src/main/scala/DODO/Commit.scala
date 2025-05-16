package DODO

import chisel3._
import chisel3.util._

class Commit extends Module {
  val io = IO(new Bundle {
    val EnA = Input(new InstCtrlBlock)
    val EnB = Input(new InstCtrlBlock)
    val CmtA = Output(new InstCtrlBlock)
    val CmtB = Output(new InstCtrlBlock)

    val FinA = Input(new InstCtrlBlock)
    val FinB = Input(new InstCtrlBlock)
    val FinC = Input(new InstCtrlBlock)
    val FinD = Input(new InstCtrlBlock)
    val FinE = Input(new InstCtrlBlock)

    val ReOrderNumA = Output(UInt(6.W))
    val ReOrderNumB = Output(UInt(6.W))

    val ForwardLoad = Input(new LoadIssue)
    val ForwardStore = Output(new StoreIssue)

    val FetchBlock = Output(Bool())
    val Rollback = Output(Bool())
  })

  // Pointer
  val EnQueuePointer = RegInit(0.U(6.W)) //队尾
  val DeQueuePointer = RegInit(0.U(6.W)) //队头
  io.FetchBlock := ((EnQueuePointer + 1.U) === DeQueuePointer) || ((EnQueuePointer + 2.U) === DeQueuePointer)
  io.ReOrderNumA := EnQueuePointer
  io.ReOrderNumB := EnQueuePointer + 1.U

  val Bank = RegInit(VecInit(Seq.fill(64)(WireInit(0.U.asTypeOf(new InstCtrlBlock())))))

  val CmtA = Bank(DeQueuePointer)
  val CmtB = Bank(DeQueuePointer + 1.U)
  val CmtBranchJump = (CmtB.isa.Bclass || CmtB.isa.Jclass).asBool
  val Aready = (CmtA.Valid && CmtA.finish).asBool

  io.Rollback := (Aready && (CmtA.jump.Valid || (CmtA.branch.Valid && CmtA.branch.actTaken.asBool))) //给actTaken加了.asBool，把外面的.asBool去掉了
  val CmtBisPrint = (CmtB.inst(6, 0) === "h7b".U(7.W)).asBool
  val CmtBisHalt = (CmtB.inst(6, 0) === "h6b".U(7.W)).asBool
  val CmtBisStore = CmtB.isa.Sclass.asBool

  val Bready = (Aready && CmtB.Valid && CmtB.finish && !CmtBranchJump && !io.Rollback && !CmtBisPrint && !CmtBisHalt && !CmtBisStore).asBool

  io.CmtA := Mux(Aready, CmtA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  io.CmtB := Mux(Bready, CmtB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))

  val Aenter = (!io.FetchBlock && io.EnA.Valid).asBool
  val Benter = (Aenter && io.EnB.Valid).asBool

  when(io.Rollback) {
    for (i <- 0 to 63) {
      Bank(i) := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    }
    EnQueuePointer := 0.U
    DeQueuePointer := 0.U
  }.otherwise {
    when(!io.FetchBlock) { Bank(EnQueuePointer) := io.EnA }
    when(!io.FetchBlock) { Bank(EnQueuePointer + 1.U) := io.EnB }
    when(io.FinA.Valid && io.FinA.finish) { Bank(io.FinA.reOrderNum) := io.FinA }
    when(io.FinB.Valid && io.FinB.finish) { Bank(io.FinB.reOrderNum) := io.FinB }
    when(io.FinC.Valid && io.FinC.finish) { Bank(io.FinC.reOrderNum) := io.FinC }
    when(io.FinD.Valid && io.FinD.finish) { Bank(io.FinD.reOrderNum) := io.FinD }
    when(io.FinE.Valid && io.FinE.finish) { Bank(io.FinE.reOrderNum) := io.FinE }
    EnQueuePointer := EnQueuePointer + Aenter.asUInt + Benter.asUInt
    DeQueuePointer := DeQueuePointer + Aready.asUInt + Bready.asUInt
    when(Aready) { Bank(DeQueuePointer) := WireInit(0.U.asTypeOf(new InstCtrlBlock())) }
    when(Bready) { Bank(DeQueuePointer + 1.U) := WireInit(0.U.asTypeOf(new InstCtrlBlock())) }
  }

  //load forward
  val HitVector = GenHitVec()
  val HIT = (HitVector =/= 0.U).asBool

  val distance = 64.U - EnQueuePointer
  val LeftHitV = CyclicShiftLeft(HitVector, distance)
  val UniqueHitV = PriorityEncoderOH(LeftHitV)
  val RightHitV = CyclicShiftRight(UniqueHitV, distance)
  val HitIndex = PriorityEncoder(RightHitV)

  io.ForwardStore := Mux(HIT, Bank(HitIndex).store, WireInit(0.U.asTypeOf(new StoreIssue())))

  def GenHitVec(): UInt = {
    val HitVec = Wire(Vec(64, UInt(1.W)))
    for (i <- 0 to 63) {
      HitVec(i) := (io.ForwardLoad.Valid && Bank(i).Valid && Bank(i).store.Valid && (io.ForwardLoad.addr(63, 3) === Bank(i).store.addr(63, 3))).asUInt
    }
    HitVec.asUInt
  }

  // 传递CSR写数据
  when(io.CmtA.isa.CSRRW) {
    io.CmtA.csr_wdata := io.CmtA.src1
  }
  when(io.CmtB.isa.CSRRW) {
    io.CmtB.csr_wdata := io.CmtB.src1
  }
}

object CyclicShiftLeft {
  def apply(vec: UInt, n: UInt): UInt = {
    ((vec >> (64.U - n))(63, 0) | (vec << n)(63, 0))
  }
}

object CyclicShiftRight {
  def apply(vec: UInt, n: UInt): UInt = {
    ((vec << (64.U - n))(63, 0) | (vec >> n)(63, 0))
  }
}