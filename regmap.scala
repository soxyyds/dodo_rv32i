// RegMap.scala
package rainbowcore

import chisel3._
import chisel3.util._

val io = IO(new Bundle{
	val in_A = Input (NEW instrblock)//输入信号传入
	val in_B = Input (NEW instrblock)
    val out_A = Output (NEW instrblock)
	val out_B = Output (new instrblock)//重命名之后指令的输出
	val enable = Input (Bool()) //使能信号 即允许指令的进入
	val rollback = Input (Bool()) //回滚信号 ，如果出现了分支预测错误，需要进行回滚的
    
	val cmt_A = Input(NEW instrblock)//两条预提交的指令的输入
	val cmt_B = Input(NEW instrblock)

	val fin_A =Input(NEW instrblock)
	val fin_B =Input (NEW instrblock)
	val fin_C =Input (NEW instrblock)
	val fin_D =Input(NEW instrblock)
	val fin_E =Input(NEW instrblock)//传入五个后面模块完成的信号，相当于前馈信号
	//有个问题这时候传出前馈信号 那时候值已经存入了么？这时候传出前馈信号完成，
	//那相应的信号存储了么？也许不用？直接从地址出发，然后找到依赖的寄存器，所以我们需要记录下这个依赖关系
	//因此寄存器不能乱释放，需要等到下一个依赖的指令完成了，上一个寄存器才能释放
    val num_A =Input (UInt(6.w))
	val num_B =Input (UInt(6.w))//重排序缓冲区编号
    val regstate = Output (UInt(128.w))//记录了128个寄存器的空闲状态
	val regvalues = Output (vec(32,UInt(64.w)))
})
    val reg_A =RegEnable(io.in_A,enable)//根据使能信号生成流水线寄存器
	val reg_B =RegEnable(io.in_B,enable)
	val inst_A = Mux(~io.FetchBlock, RegA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
	val inst_B = = Mux(~io.FetchBlock, RegB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
	//首先要初始化好一些要用的记录表，并且这些模块需要提前自定义好
	//如1：重命名的映射表 就是32个架构寄存器到128个（7位宽）物理寄存器的映射关系
	//2 ：提交表记录下映射和依赖关系，用于回滚操作
	//3 ：架构寄存器的值
	//4：物理寄存器的状态表（是否处于可以分配的状态）
	//首先定义好初始的映射模块,并且定义好方法，从而可以读取和修改内部的映射关系
	class MapBank (depth: Int, width: Int){
		val map= VecInit(0.U(7.W),1.U(7.W),2.U(7.W),3.U(7.W),4.U(7.W),5.U(7.W),6.U(7.W),7.U(7.W),8.U(7.W),9.U(7.W),10.U(7.W),11.U(7.W),12.U(7.W),13.U(7.W),14.U(7.W),15.U(7.W),16.U(7.W),17.U(7.W),18.U(7.W),19.U(7.W),20.U(7.W),21.U(7.W),22.U(7.W),23.U(7.W),24.U(7.W),25.U(7.W),26.U(7.W),27.U(7.W),28.U(7.W),29.U(7.W),30.U(7.W),31.U(7.W))
        val table = RegInit(map)
		def read (num:UInt):UInt={table(num)}
		def write (enable:Bool,num:UInt ,data :UInt):UInt ={
			when (enable && num =/=0.U) {table(num):=data}
		}
	}
	val MapTable = new Mapbank(32, 7)//MapTable完成了建立
	//下一步是已经提交的指令的记录表，用于回滚操作,提交表和映射表是同一个结构体
	//回滚的时候可以直接往上一置换
	val cmtable = new Mapbank (32,7) 
	//下一步定义 架构寄存器表，储存架构寄存器的值
	class regvaluebank(depth:Int ,width:Int){
		val table = RegInit(VecInit(seq.fill(depth)(0.U(width.W))))//这里是存储的是值
		def read (num : UInt):UInt = {table(num)}
		def write (enable:Bool,num:UInt,data:UInt):Unit={
			when(enable && num =/=0.U){table(num):=data}
		}
	}
	val regvalues = new regvaluebank(32,32)
	//这里不知道我们的平台是RISCV32么
	//下一步定义物理寄存器表状态,这里面应该涉及有read和唤醒
	class regstatebank {
		首先定义出四个状态
	// Free: 00
    // Mapped: 01
    // Executed: 10
    // retired: 11
	//定义好复位的状态，一定是32个物理寄存器正在被使用,高的96free 低的32占用
		val seqassign = seq.fill(32)(3.U(2.W))
		val seqfree =seq.fill (96)(0.U(2.w))
		val seqstate = seqassign ++ seqfree
		val regstate = RegInit(VecInit(seqstate))
		//首先要书写分配逻辑
		val freelist = genfreelist()
		def genfreelist():UInt ={
			val freelist = Wire(Vec(128, UInt(1.W)))
			for(i <- 0to 127){
				freelist(i) := regstate == 0.U
			}
			freelist.asUInt
		}//传回去128位宽的二进制码
		val newfreelist = freelist - lowbit(freelist)
		def freereg_A ():UInt = {Log2(lowbit(freelist))}
		def freereg_B ():UInt = {Log2(lowbit(newfreelist))}
        
		def avalist():UInt ={
			val avalist = Wire (Vec(128,UInt(1.w)))
			for(i <- 0 to 127){
				ava(i) := regstate(i)(1)
			}
			avalist.asUInt
		}
		def write(enable:Bool,num:Bool,data:UInt):Unit={
			when(enable&&num=/=0.U){
				regstate(num):=data
			}
		}
		def rollback(free_num:UInt,retired_num:UInt): Unit = {
            for(i <- 1 to 127 ){
				when (i.U ==free_num){
					regstate(i) := 0.U
				}.elsewhen(i.U == retired_num){
					regstate(i) := 3.U
				}.otherwise{
					when(regstate(i) == 3.U){
						regstate(i) := 3.U
					}.otherwise{
						regstate(i) := 0.U
					}
				}
			}
		}//回滚的重置逻辑
	}
	val regstate = new regstatebank 
	//基础的初始逻辑完成了
	接下来我们需要完成功能的实现了
	//功能1 ：完成相应的物理寄存器的重命名分配
	val regfree_A = wire (UInt())
	val regfree_B = wire (UInt())
	when (in_A.redges == 0.U){
		regfree_A := 0.U
	}.otherwise{
		regfree_A :=regstate.freereg_A
	}
	when (in_B.redges == 0.U){
		regfree_B := 0.U
	}.otherwise{
		regfree_B :=regstate.freereg_B
	}//即为进入的指令的A和B的目标寄存器分配好了对应的寄存器
    //功能2 ： 读取旧的依赖关系，处理掉WAW依赖
	val old_depend_A = MapTable.read(in_A.redges)//就是读取当前这个传入指令的目标寄存器依赖的寄存器
	val old_depend_B = if(in_A.redges == in_B.redges) old_depend_A else MapTable.read(in_B.redges)
    //这个返回的是目前这个寄存器依赖的物理寄存器，因为指令完成之后并不能直接释放，需要等下一个物理寄存器retired之后才能释放物理寄存器
	val regrs1_A = MapTable.read(in_A.regsrc1)
	val regrs2_A = MapTable.read(in_A.regsrc2)
	val regrs1_B = if(in_A.redges == in_B.regsrc1) old_depend_A else MapTable.read(in_B.regsrc1)
	val regrs2_B = if(in_A.redges == in_B.regsrc2) old_depend_A else MapTable.read(in_B.regsrc2)
    io.regstate := regstate.avalist
	io.regvalues := regvalues.table//将架构寄存器的值传输出去
	
	//功能3：对提交到这个regmap模块里面的已经提交的指令进行处理。即AB两条指令退休了，要处理依赖的那些寄存器
	//首先如果AB两个指令的目标寄存器是一样的话，我们不需要提交A，如果不一样就要提交
	when (io.cmt_A.redges=/=io.cmt_B/redges){
		cmtable.write(io.cmt_A.Valid,io.cmt_A.redges,io.cmt_A.pregdes)//仅仅只要更新retird的A信息即可
		regvalues.write(io.cmt_A.Valid,io.cmt_A.redges,io.cmt_A.wbdata)//更新对应的架构寄存器
	}
	cmtable.write(io.cmt_B.Valid,io.cmt_B.redges,io.cmt_B.pregdes)//仅仅只要更新retird的A信息即可
	regvalues.write(io.cmt_B.Valid,io.cmt_B.redges,io.cmt_B.wbdata)//更新对应的架构寄存器
    
	regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.pregdes, 3.U(2.W))
    regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.pregdes, 3.U(2.W))//
	//上面的是去刷新新提交的寄存器的状态为已经提交
	regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.cmtdes, 0.U(2.W))
    regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.cmtdes, 0.U(2.W))
    //然后我们再去将对应的上一个依赖的物理寄存器的状态从已经提交刷新到free,从而完成了cmt指令的作用

	//功能4：回滚处理
	when(io.rollback){
		//首先把所有的寄存器的映射换成已经提交的映射
		for(i <- 0 ro 31){
			MapTable.table(i) := cmtable.table(i)
		}//但是有个问题就是回滚的时候 同时提交了A和B两个指令，而AB刷新这个cmtable和回滚的when是并行的，所以需要保护这两个
		when(io.cmt_A.Valid && io.cmt_A.regdes =/= 0.U) {
            MapTable.table(io.cmt_A.regdes) := io.cmt_A.pregdes
        }
		when(io.cmt_B.Valid && io.cmt_B.redges =/= 0.U) {
			MapTable.table(io.cmt_B.regdes) := io.cmt_B.pregdes
		}
		regstate.rollback(io.cmt_A.cmtdes,io.cmt_A.pregdes)
		regstate.rollback(io.cmt_B.cmtdes,io.cmt_B.pregdes)
		io.out_A := WireInit(0.U.asTypeOf(new InstCtrlBlock()))
		io.out_B := WireInit(0.U.asTypeOf(new InstCtrlBlock()))	
	}.otherwise{
		//不回滚的情况下，我们则需要更新新的映射关系，这里便是映射关系的书写
		//需要完成三个事情1：寄存器映射关系的更新 2：物理寄存器状态的管理 3：输出重命名之后的指令
		when (inst_A =/= inst_B){
            MapTable.write (inst_A.Valid,inst_A,redges,regfree_A )
		}
		MapTable.write (inst_B.Valid,inst_B,redges,regfree_B )//分配了新的物理寄存器给它，这个关系要更新
		regstate.write(regfree_A=/=0.U,regfree_A,1.U(2.w))
		regstate.write(regfree_B=/=0.U,regfree_B,1.U(2.w))//刷洗寄存器的占用状态表

		//前馈信号来唤醒寄存器的状态，将寄存器从占用状态唤醒到执行完成状态
		regstate.write(io.fin_A.Valid && io.fin_A.finish, io.fin_A.pregdes, 2.U(2.W))
		regstate.write(io.fin_B.Valid && io.fin_B.finish, io.fin_B.pregdes, 2.U(2.W))
		regstate.write(io.fin_C.Valid && io.fin_C.finish, io.fin_C.pregdes, 2.U(2.W))
		regstate.write(io.fin_D.Valid && io.fin_D.finish, io.fin_D.pregdes, 2.U(2.W))
		regstate.write(io.fin_E.Valid && io.fin_E.finish, io.fin_E.pregdes, 2.U(2.W))

		//最后重新组装新的指令数据块
		io.out_A :=genall(regrs1_A,regrs2_A,old_depend_A,num_A,inst_A)
		io.out_B :=genall(regrs1_B,regrs2_B,old_depend_B,num_B,inst_B)
	}
    def genall (presrc1:UInt,presrc2:UInt,predges:UInt,cmtdes:UInt,reorderNum:UInt,inst_in:InstCtrlBlock):InstCtrlBlock = {
		val inst_end = wire(NEW InstCtrlBlock)
		inst_end.Valid :=inst_in.Valid
		inst_end.inst :=inst_in.inst
		inst_end.pc := inst_in.pc
		inst_end.isa := inst_in.isa
		inst_end.finish := inst_in.finish
		inst_end.reorderNum := reorderNum//传回来顺序
		inst_end.redges := inst_in.redges
		inst_end.regsrc1 := inst_in.regsrc1
		inst_end.regsrc2 := inst_in.regsrc2
		inst_end.presrc1 := presrc1//重命名加了个映射
		inst_end.presrc2 := presrc2//重命名加了个映射
		inst_end.predges := predges//重命名加了个映射
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
        data & (~data+1.U)
    }
}//辅助函数，保留最低位的1
//input : 00101010110
//output: 00100000000
object highbit {
    def apply(data: UInt): UInt = {
        Reverse(lowbit(Reverse(data)))
    }
}