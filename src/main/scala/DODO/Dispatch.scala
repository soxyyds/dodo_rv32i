//Dispatch.scala
package DODO

import chisel3._
import chisel3.util._

class dispatch extends Module{
  val io = IO(new Bundle {
    val in_A = Input(new InstCtrlBlock)
    val in_B = Input(new InstCtrlBlock)
    val out_A = Output(new InstCtrlBlock)
    val out_B = Output(new InstCtrlBlock)
    val out_C = Output(new InstCtrlBlock)
    //此处需要将指令进行分类，于是输出的指令实际上是三条out
    //在这里面需要建立两个发射队列reserve 这个保留站的发送需要根据依赖的寄存器的状态来判断的
    //于是输入的肯定需要寄存器的状态表 还有回滚信号 还有肯定还有根据保留站里面指令的数量的使能信号
    val regstate = Input(UInt(128.W))
    val fetchblock = Output(Bool())
    val rollback = Input(Bool())
  })
  //在成功创建好两个保留站，现在需要实例化，并且成相应的逻辑
  val intquene: intquene = Module (new intquene)
  val memquene: memquene = Module(new memquene)
  //然后需要将指令分配到两个保留站里面
  def opcode(inst: InstCtrlBlock): UInt = inst.inst(6, 0)
  def ismem(inst: InstCtrlBlock): Bool = {
    val op = opcode(inst)
    op === "b0000011".U || op === "b0100011".U  // Load或Store指令
  }
  def isint(inst: InstCtrlBlock): Bool = !ismem(inst)
  when(isint(io.in_A)){
    intquene.io.intquene_in_A <> io.in_A
    memquene.io.memquene_in_A <> WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    intquene.io.intquene_in_A <> WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    memquene.io.memquene_in_A <> io.in_A
  }
  when(isint(io.in_B)){
    intquene.io.intquene_in_B <> io.in_B
    memquene.io.memquene_in_B <> WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    intquene.io.intquene_in_B <> WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    memquene.io.memquene_in_B <> io.in_B
  }
  io.out_A := intquene.io.intquene_out_A
  io.out_B := intquene.io.intquene_out_B
  io.out_C := memquene.io.memquene_out_C
  intquene.io.regstate <> io.regstate
  memquene.io.regstate <> io.regstate
  intquene.io.rollback <> io.rollback
  memquene.io.rollback <> io.rollback
  io.fetchblock := (intquene.io.intfull || memquene.io.memfull)
}
//首先创建两个保留站
//1：整形保留站
class intquene extends Module{
  val io = IO (new Bundle{
    val intquene_in_A = Input(new InstCtrlBlock)
    val intquene_in_B = Input(new InstCtrlBlock)
    val intquene_out_A = Output(new InstCtrlBlock)
    val intquene_out_B = Output(new InstCtrlBlock)
    val regstate = Input(UInt(128.W))
    val rollback = Input(Bool())
    val intfull = Output(Bool())
  })
  //在这个保留站里面，我们很需要指向标还有存储站，指令标有两个，一个为入队另一个则为出队,还有堆满的信号
  val reserve: Vec[InstCtrlBlock]  = RegInit(VecInit(Seq.fill(16)(WireInit(0.U.asTypeOf(new InstCtrlBlock())))))
  //首先把16个槽位的空闲状态通过genfreelist用16位01指令表示出来，然后取最低的即为进队的point，并且取2对数
  val freelist_A: UInt = genfreelist_A()
  val freelist_B: UInt = freelist_A - lowbit(freelist_A)
  val in_point_A: UInt = Log2(lowbit(freelist_A))
  val in_point_B: UInt = Log2(lowbit(freelist_B))
  io.intfull := (lowbit(freelist_A) === 0.U) || (lowbit(freelist_B) === 0.U)
  //然后出队的指针需要根据就绪状态来判定，于是通过genreadylist函数判定处于槽位中的指令的ready状态，发送ready的即可，后面可考虑加入一个年龄判定优先级
  val readylist_A: UInt = genreadylist_A()
  val readylist_B: UInt = readylist_A  - lowbit(readylist_A)
  val out_point_A: UInt = Log2(lowbit(readylist_A))
  val out_point_B: UInt = Log2(lowbit(readylist_B))

  def genfreelist_A(): UInt = {
    val freelist = Wire(Vec(16, UInt(1.W)))
    for(i <- 0 to 15){
      freelist(i) := ~reserve(i).Valid
    }
    freelist.asUInt
  }//readylist需要查看代码的有效性还有依赖读的两个物理寄存器的状态是否是执行完的
  def genreadylist_A(): UInt = {
    val readylist = Wire(Vec(16,UInt(1.W)))
    for(i <- 0 to 15){
      readylist(i) := reserve(i).Valid && io.regstate(reserve(i).pregsrc1)&& io.regstate(reserve(i).pregsrc2)
    }
    readylist.asUInt
  }

  //接下来就是根据是回滚还是正常去将指令插入保留站
  //如果是回滚就清空保留站，并且这个时序就不会输出对应的指令的out是一个空指令
  when(io.rollback){
    for(i <- 0 to 15){
      reserve(i) := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    }
    io.intquene_out_A :=  WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.intquene_out_B :=  WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    reserve(in_point_A) := io.intquene_in_A
    reserve(in_point_B) := io.intquene_in_B

    when(lowbit(readylist_A) =/=0.U){
      io.intquene_out_A := reserve (out_point_A)
      reserve(out_point_A) := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    }.otherwise{io.intquene_out_A :=WireInit(0.U.asTypeOf(new InstCtrlBlock())) }
    when(lowbit(readylist_B) =/=0.U){
      io.intquene_out_B := reserve (out_point_B)
      reserve(out_point_B) := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    }.otherwise{io.intquene_out_B :=WireInit(0.U.asTypeOf(new InstCtrlBlock()))}
  }
  //这个是用于阻塞判断的，从而避免满状态的出现
  //  freelist_A := genfreelist_A()
  //freelist_B := freelist_A - lowbit(freelist_A)
  //  in_point_A := Log2(lowbit(freelist_A))
  // in_point_B := Log2(lowbit(freelist_B))

}
//2：访存保留站
class memquene extends Module{
  val io = IO (new Bundle{
    val memquene_in_A = Input(new InstCtrlBlock)
    val memquene_in_B = Input(new InstCtrlBlock)
    val memquene_out_C =Output(new InstCtrlBlock)

    val regstate = Input(UInt(128.W))
    val memfull = Output(Bool())
    val rollback = Input(Bool())
  })
  //访存保留站同样需要有储存器 还有入队还有进队的指针,但是这个指针略有不同因为最后采用的是环形逻辑
  val reserve: Vec[InstCtrlBlock] = RegInit(VecInit(Seq.fill(16)(WireInit(0.U.asTypeOf(new InstCtrlBlock())))))
  val in_point: UInt = RegInit(0.U(4.W))
  val out_point: UInt= RegInit(0.U(4.W))
  io.memfull := ((in_point + 1.U ) === out_point || (in_point + 2.U ) === out_point)

  when(io.rollback) {
    in_point := 0.U
    out_point := 0.U
    for(i <- 0 to 15) {
      reserve(i) := 0.U.asTypeOf(new InstCtrlBlock())  // 清除所有条目
    }
  }.elsewhen(io.memquene_in_A.Valid && io.memquene_in_B.Valid) {
    reserve(in_point) := io.memquene_in_A
    reserve(in_point + 1.U) := io.memquene_in_B
    in_point := in_point + 2.U
  }.elsewhen(!io.memquene_in_A.Valid && io.memquene_in_B.Valid) {
    reserve(in_point) := io.memquene_in_B
    in_point := in_point + 1.U
  }.elsewhen(io.memquene_in_A.Valid && !io.memquene_in_B.Valid) {
    reserve(in_point) := io.memquene_in_A
    in_point := in_point + 1.U
  }
  //这里out_point指针指向的是要出站的指令位置，但是要出去必须就绪才可以出去，于是要看它的物理寄存器是否就绪
  when(!io.rollback && (reserve(out_point).Valid && io.regstate(reserve(out_point).pregsrc1)&&io.regstate(reserve(out_point).pregsrc2))){
    io.memquene_out_C := reserve(out_point)
    reserve(out_point) := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    out_point := out_point + 1.U
  }.otherwise{
    io.memquene_out_C := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }
}
object lowbit {
  def apply(data: UInt): UInt = {
    val result = data & (-data).asTypeOf(UInt(data.getWidth.W))
    result(data.getWidth-1, 0)
  }
}



