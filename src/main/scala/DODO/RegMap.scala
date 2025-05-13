// regmap.scala
package DODO

import chisel3._
import chisel3.util._

class RegMap extends Module{
  val io = IO(new Bundle{
    val in_A = Input (new InstCtrlBlock)//输入信号传入
    val in_B = Input (new InstCtrlBlock)
    val out_A = Output (new InstCtrlBlock)
    val out_B = Output (new InstCtrlBlock)//重命名之后指令的输出
    val enable = Input (Bool()) //使能信号 即允许指令的进入
    val rollback = Input (Bool()) //回滚信号 ，如果出现了分支预测错误，需要进行回滚的

    val cmt_A = Input(new InstCtrlBlock)//两条预提交的指令的输入
    val cmt_B = Input(new InstCtrlBlock)

    val fin_A =Input(new InstCtrlBlock)
    val fin_B =Input (new InstCtrlBlock)
    val fin_C =Input (new InstCtrlBlock)
    val fin_D =Input(new InstCtrlBlock)
    val fin_E =Input(new InstCtrlBlock)//传入五个后面模块完成的信号，相当于前馈信号
    //有个问题这时候传出前馈信号 那时候值已经存入了么？这时候传出前馈信号完成，
    //那相应的信号存储了么？也许不用？直接从地址出发，然后找到依赖的寄存器，所以我们需要记录下这个依赖关系
    //因此寄存器不能乱释放，需要等到下一个依赖的指令完成了，上一个寄存器才能释放
    val num_A =Input (UInt(6.W))
    val num_B =Input (UInt(6.W))//重排序缓冲区编号
    val regstate : UInt = Output (UInt(128.W))//记录了128个寄存器的空闲状态
    val regvalues : Vec[UInt]= Output (Vec(32,UInt(64.W)))
  })
  val reg_A:InstCtrlBlock =RegEnable(io.in_A,io.enable)//根据使能信号生成流水线寄存器
  val reg_B:InstCtrlBlock =RegEnable(io.in_B,io.enable)
  val inst_A:InstCtrlBlock = Mux(io.enable, reg_A , WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  val inst_B:InstCtrlBlock = Mux(io.enable, reg_B, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
  //首先要初始化好一些要用的记录表，并且这些模块需要提前自定义好
  //如1：重命名的映射表 就是32个架构寄存器到128个（7位宽）物理寄存器的映射关系
  //2 ：提交表记录下映射和依赖关系，用于回滚操作
  //3 ：架构寄存器的值
  //4：物理寄存器的状态表（是否处于可以分配的状态）
  //首先定义好初始的映射模块,并且定义好方法，从而可以读取和修改内部的映射关系
  class InitIncreaseRegBank(depth: Int, width: Int) {
    val map: Vec[UInt] = VecInit(Seq.tabulate(depth)(i => i.U(width.W)))
    val table: Vec[UInt] = RegInit(map)
    def read(num: UInt): UInt = table(num)
    def write(enable: Bool, num: UInt, data: UInt): Unit = {
      when(enable && num =/= 0.U) {
        table(num) := data
      }
    }
  }
  val MapTable:InitIncreaseRegBank = new InitIncreaseRegBank(32, 7)//MapTable完成了建立
  //下一步是已经提交的指令的记录表，用于回滚操作,提交表和映射表是同一个结构体
  //回滚的时候可以直接往上一置换
  val cmtable:InitIncreaseRegBank = new InitIncreaseRegBank (32,7)
  //下一步定义 架构寄存器表，储存架构寄存器的值
  class AbstractRegBank(depth:Int ,width:Int){
    val table: Vec[UInt] = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))//这里是存储的是值
    def read (num : UInt):UInt = {table(num)}
    def write (enable:Bool,num:UInt,data:UInt):Unit={
      when(enable && num =/=0.U){table(num):=data}
    }
  }
  val regvalues:AbstractRegBank = new AbstractRegBank(32,32)
  //这里不知道我们的平台是RISCV32么
  //下一步定义物理寄存器表状态,这里面应该涉及有read和唤醒
  class PhyRegStatesTable {
    // Free: 00
    // Mapped: 01
    // Executed: 10
    // retired: 11
    val SeqAssigned: Seq[UInt] = Seq.fill(32)(3.U(2.W))  // 低32个寄存器状态为11
    val SeqFree: Seq[UInt] = Seq.fill(96)(0.U(2.W))      // 高96个寄存器状态为00
    val SeqInit: Seq[UInt] = SeqAssigned ++ SeqFree      // 合并序列
    val PhyRegStates: Vec[UInt] = RegInit(VecInit(SeqInit))
    def genfreelist(): UInt = {
      val freelist = Wire(Vec(128, UInt(1.W)))
      for(i <- 0 to 127){
        freelist(i) := (PhyRegStates(i) === 0.U).asUInt()
      }
      freelist.asUInt
    }
    val freelist: UInt  = genfreelist()
    val newfreelist: UInt  = freelist & (~lowbit(freelist)).asUInt()
    def freereg_A(): UInt = { Log2(lowbit(freelist)) }
    def freereg_B(): UInt = { Log2(lowbit(newfreelist))}
    def avalist(): UInt = {
      val avalist: Vec[UInt]  = Wire(Vec(128, UInt(1.W)))
      for(i <- 0 to 127){
        avalist(i) := PhyRegStates(i)(1)
      }
      avalist.asUInt
    }
    def write(enable: Bool, num: UInt, data: UInt): Unit = {  // 修正num类型为UInt
      when(enable && num =/= 0.U){ PhyRegStates(num) := data
      }}
    def rollback(free_num: UInt, retired_num: UInt): Unit = {
      for(i <- 1 to 127){
        when(i.U === free_num){
          PhyRegStates(i) := 0.U
        }.elsewhen(i.U === retired_num){
          PhyRegStates(i) := 3.U
        }.otherwise{
          when(PhyRegStates(i) === 3.U){
            PhyRegStates(i) := 3.U
          }.otherwise{
            PhyRegStates(i) := 0.U
          }}}}
  }
  val regstate: PhyRegStatesTable = new PhyRegStatesTable
  //基础的初始逻辑完成了
  //接下来我们需要完成功能的实现了
  //功能1 ：完成相应的物理寄存器的重命名分配
  val regfree_A: UInt  = Wire (UInt())
  val regfree_B: UInt  = Wire (UInt())
  when (io.in_A.regdes =/= 0.U){
    regfree_A := regstate.freereg_A
  }.otherwise{
    regfree_A := 0.U
  }
  when (io.in_B.regdes =/= 0.U){
    regfree_B := regstate.freereg_B
  }.otherwise{
    regfree_B := 0.U
  }//即为进入的指令的A和B的目标寄存器分配好了对应的寄存器
  //功能2 ： 读取旧的依赖关系，处理掉WAW依赖
  val old_depend_A: UInt  = MapTable.read(io.in_A.regdes)//就是读取当前这个传入指令的目标寄存器依赖的寄存器
  val old_depend_B: UInt  = if(io.in_A.regdes == io.in_B.regdes) old_depend_A else MapTable.read(io.in_B.regdes)
  //这个返回的是目前这个寄存器依赖的物理寄存器，因为指令完成之后并不能直接释放，需要等下一个物理寄存器retired之后才能释放物理寄存器
  val regrs1_A: UInt  = MapTable.read(io.in_A.regsrc1)
  val regrs2_A: UInt  = MapTable.read(io.in_A.regsrc2)
  val regrs1_B: UInt  = if(io.in_A.regdes == io.in_B.regsrc1) old_depend_A else MapTable.read(io.in_B.regsrc1)
  val regrs2_B: UInt  = if(io.in_A.regdes == io.in_B.regsrc2) old_depend_A else MapTable.read(io.in_B.regsrc2)
  io.regstate := regstate.avalist
  io.regvalues := regvalues.table//将架构寄存器的值传输出去

  //功能3：对提交到这个regmap模块里面的已经提交的指令进行处理。即AB两条指令退休了，要处理依赖的那些寄存器
  //首先如果AB两个指令的目标寄存器是一样的话，我们不需要提交A，如果不一样就要提交
  when (io.cmt_A.regdes=/=io.cmt_B.regdes){
    cmtable.write(io.cmt_A.Valid,io.cmt_A.regdes,io.cmt_A.pregdes)//仅仅只要更新retird的A信息即可
    regvalues.write(io.cmt_A.Valid,io.cmt_A.regdes,io.cmt_A.wbdata)//更新对应的架构寄存器
  }
  cmtable.write(io.cmt_B.Valid,io.cmt_B.regdes,io.cmt_B.pregdes)//仅仅只要更新retird的A信息即可
  regvalues.write(io.cmt_B.Valid,io.cmt_B.regdes,io.cmt_B.wbdata)//更新对应的架构寄存器

  regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.pregdes, 3.U(2.W))
  regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.pregdes, 3.U(2.W))//
  //上面的是去刷新新提交的寄存器的状态为已经提交
  regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.cmtdes, 0.U(2.W))
  regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.cmtdes, 0.U(2.W))
  //然后我们再去将对应的上一个依赖的物理寄存器的状态从已经提交刷新到free,从而完成了cmt指令的作用

  //功能4：回滚处理
  when(io.rollback){
    //首先把所有的寄存器的映射换成已经提交的映射
    for(i <- 0 to 31){
      MapTable.table(i) := cmtable.table(i)
    }//但是有个问题就是回滚的时候 同时提交了A和B两个指令，而AB刷新这个cmtable和回滚的when是并行的，所以需要保护这两个
    when(io.cmt_A.Valid && io.cmt_A.regdes =/= 0.U) {
      MapTable.table(io.cmt_A.regdes) := io.cmt_A.pregdes
    }
    when(io.cmt_B.Valid && io.cmt_B.regdes =/= 0.U) {
      MapTable.table(io.cmt_B.regdes) := io.cmt_B.pregdes
    }
    regstate.rollback(io.cmt_A.cmtdes,io.cmt_A.pregdes)
    regstate.rollback(io.cmt_B.cmtdes,io.cmt_B.pregdes)
    io.out_A := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
    io.out_B := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
  }.otherwise{
    //不回滚的情况下，我们则需要更新新的映射关系，这里便是映射关系的书写
    //需要完成三个事情1：寄存器映射关系的更新 2：物理寄存器状态的管理 3：输出重命名之后的指令
    when (InstCtrlBlock.=/=(inst_A, inst_B)){//这里注意二者无法直接比较
      MapTable.write (inst_A.Valid,inst_A.regdes,regfree_A )
    }
    MapTable.write (inst_B.Valid,inst_B.regdes,regfree_B )//分配了新的物理寄存器给它，这个关系要更新
    regstate.write(regfree_A=/=0.U,regfree_A,1.U(2.W))
    regstate.write(regfree_B=/=0.U,regfree_B,1.U(2.W))//刷洗寄存器的占用状态表

    //前馈信号来唤醒寄存器的状态，将寄存器从占用状态唤醒到执行完成状态
    regstate.write(io.fin_A.Valid && io.fin_A.finish, io.fin_A.pregdes, 2.U(2.W))
    regstate.write(io.fin_B.Valid && io.fin_B.finish, io.fin_B.pregdes, 2.U(2.W))
    regstate.write(io.fin_C.Valid && io.fin_C.finish, io.fin_C.pregdes, 2.U(2.W))
    regstate.write(io.fin_D.Valid && io.fin_D.finish, io.fin_D.pregdes, 2.U(2.W))
    regstate.write(io.fin_E.Valid && io.fin_E.finish, io.fin_E.pregdes, 2.U(2.W))

    //最后重新组装新的指令数据块
    io.out_A :=genall(regrs1_A,regrs2_A,old_depend_A,regfree_A,io.num_A,inst_A)
    io.out_B :=genall(regrs1_B,regrs2_B,old_depend_B,regfree_B,io.num_B,inst_B)
  }
  def genall (pregsrc1:UInt,pregsrc2:UInt,pregdes:UInt,cmtdes:UInt,reOrderNum:UInt,inst_in:InstCtrlBlock):InstCtrlBlock = {
    val inst_end = Wire(new InstCtrlBlock)
    inst_end.Valid :=inst_in.Valid
    inst_end.inst :=inst_in.inst
    inst_end.pc := inst_in.pc
    inst_end.isa := inst_in.isa
    inst_end.finish := inst_in.finish
    inst_end.reOrderNum := reOrderNum//传回来顺序
    inst_end.regdes := inst_in.regdes
    inst_end.regsrc1 := inst_in.regsrc1
    inst_end.regsrc2 := inst_in.regsrc2
    inst_end.pregsrc1 := pregsrc1//重命名加了个映射
    inst_end.pregsrc2 := pregsrc2//重命名加了个映射
    inst_end.pregdes := pregdes//重命名加了个映射
    inst_end.cmtdes := cmtdes//加上来依赖关系
    inst_end.src1 := inst_in.src1
    inst_end.src2 := inst_in.src2
    inst_end.imm := inst_in.imm
    inst_end.wbdata := inst_in.wbdata
    inst_end.jump := inst_in.jump
    inst_end.branch := inst_in.branch
    inst_end.load := inst_in.load
    inst_end.store := inst_in.store
    inst_end
  }
  object lowbit {
    def apply(data: UInt): UInt = {
      val result = data & (-data).asUInt()
      result(data.getWidth-1, 0)
    }
  }
  //辅助函数，保留最低位的1
  //input : 00101010110
  //output: 00100000000
  object highbit {
    def apply(data: UInt): UInt = {
      Reverse(lowbit(Reverse(data)))}}
}
object InstCtrlBlock {
  def =/=(a: InstCtrlBlock, b: InstCtrlBlock): Bool = {
    a.asUInt =/= b.asUInt  // 整体比较
  }
}
