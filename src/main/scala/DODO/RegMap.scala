package DODO


import chisel3._
import chisel3.util._

val io = IO(new Bundle{
  val in_A = Input (NEW instrblock)//�����źŴ���
  val in_B = Input (NEW instrblock)
  val out_A = Output (NEW instrblock)
  val out_B = Output (new instrblock)//������֮��ָ������
  val enable = Input (Bool()) //ʹ���ź� ������ָ��Ľ���
  val rollback = Input (Bool()) //�ع��ź� ����������˷�֧Ԥ�������Ҫ���лع���

  val cmt_A = Input(NEW instrblock)//����Ԥ�ύ��ָ�������
  val cmt_B = Input(NEW instrblock)

  val fin_A =Input(NEW instrblock)
  val fin_B =Input (NEW instrblock)
  val fin_C =Input (NEW instrblock)
  val fin_D =Input(NEW instrblock)
  val fin_E =Input(NEW instrblock)//�����������ģ����ɵ��źţ��൱��ǰ���ź�
  //�и�������ʱ�򴫳�ǰ���ź� ��ʱ��ֵ�Ѿ�������ô����ʱ�򴫳�ǰ���ź���ɣ�
  //����Ӧ���źŴ洢��ô��Ҳ���ã�ֱ�Ӵӵ�ַ������Ȼ���ҵ������ļĴ���������������Ҫ��¼�����������ϵ
  //��˼Ĵ����������ͷţ���Ҫ�ȵ���һ��������ָ������ˣ���һ���Ĵ��������ͷ�
  val num_A =Input (UInt(6.w))
  val num_B =Input (UInt(6.w))//�����򻺳������
  val regstate = Output (UInt(128.w))//��¼��128���Ĵ����Ŀ���״̬
  val regvalues = Output (vec(32,UInt(64.w)))
})
val reg_A =RegEnable(io.in_A,enable)//����ʹ���ź�������ˮ�߼Ĵ���
val reg_B =RegEnable(io.in_B,enable)
val inst_A = Mux(~io.FetchBlock, RegA, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
val inst_B = = Mux(~io.FetchBlock, RegB, WireInit(0.U.asTypeOf(new InstCtrlBlock())))
//����Ҫ��ʼ����һЩҪ�õļ�¼��������Щģ����Ҫ��ǰ�Զ����
//��1����������ӳ��� ����32���ܹ��Ĵ�����128����7λ������Ĵ�����ӳ���ϵ
//2 ���ύ���¼��ӳ���������ϵ�����ڻع�����
//3 ���ܹ��Ĵ�����ֵ
//4������Ĵ�����״̬���Ƿ��ڿ��Է����״̬��
//���ȶ���ó�ʼ��ӳ��ģ��,���Ҷ���÷������Ӷ����Զ�ȡ���޸��ڲ���ӳ���ϵ
class MapBank (depth: Int, width: Int){
  val map= VecInit(0.U(7.W),1.U(7.W),2.U(7.W),3.U(7.W),4.U(7.W),5.U(7.W),6.U(7.W),7.U(7.W),8.U(7.W),9.U(7.W),10.U(7.W),11.U(7.W),12.U(7.W),13.U(7.W),14.U(7.W),15.U(7.W),16.U(7.W),17.U(7.W),18.U(7.W),19.U(7.W),20.U(7.W),21.U(7.W),22.U(7.W),23.U(7.W),24.U(7.W),25.U(7.W),26.U(7.W),27.U(7.W),28.U(7.W),29.U(7.W),30.U(7.W),31.U(7.W))
  val table = RegInit(map)
  def read (num:UInt):UInt={table(num)}
  def write (enable:Bool,num:UInt ,data :UInt):UInt ={
    when (enable && num =/=0.U) {table(num):=data}
  }
}
val MapTable = new Mapbank(32, 7)//MapTable����˽���
//��һ�����Ѿ��ύ��ָ��ļ�¼�����ڻع�����,�ύ���ӳ�����ͬһ���ṹ��
//�ع���ʱ�����ֱ������һ�û�
val cmtable = new Mapbank (32,7)
//��һ������ �ܹ��Ĵ���������ܹ��Ĵ�����ֵ
class regvaluebank(depth:Int ,width:Int){
  val table = RegInit(VecInit(seq.fill(depth)(0.U(width.W))))//�����Ǵ洢����ֵ
  def read (num : UInt):UInt = {table(num)}
  def write (enable:Bool,num:UInt,data:UInt):Unit={
    when(enable && num =/=0.U){table(num):=data}
  }
}
val regvalues = new regvaluebank(32,32)
//���ﲻ֪�����ǵ�ƽ̨��RISCV32ô
//��һ����������Ĵ�����״̬,������Ӧ���漰��read�ͻ���
class regstatebank {
  ���ȶ�����ĸ�״̬
  // Free: 00
  // Mapped: 01
  // Executed: 10
  // retired: 11
  //����ø�λ��״̬��һ����32������Ĵ������ڱ�ʹ��,�ߵ�96free �͵�32ռ��
  val seqassign = seq.fill(32)(3.U(2.W))
  val seqfree =seq.fill (96)(0.U(2.w))
  val seqstate = seqassign ++ seqfree
  val regstate = RegInit(VecInit(seqstate))
  //����Ҫ��д�����߼�
  val freelist = genfreelist()
  def genfreelist():UInt ={
    val freelist = Wire(Vec(128, UInt(1.W)))
    for(i <- 0to 127){
      freelist(i) := regstate == 0.U
    }
    freelist.asUInt
  }//����ȥ128λ��Ķ�������
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
  }//�ع��������߼�
}
val regstate = new regstatebank
//�����ĳ�ʼ�߼������
������������Ҫ��ɹ��ܵ�ʵ����
//����1 �������Ӧ������Ĵ���������������
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
}//��Ϊ�����ָ���A��B��Ŀ��Ĵ���������˶�Ӧ�ļĴ���
//����2 �� ��ȡ�ɵ�������ϵ�������WAW����
val old_depend_A = MapTable.read(in_A.redges)//���Ƕ�ȡ��ǰ�������ָ���Ŀ��Ĵ��������ļĴ���
val old_depend_B = if(in_A.redges == in_B.redges) old_depend_A else MapTable.read(in_B.redges)
//������ص���Ŀǰ����Ĵ�������������Ĵ�������Ϊָ�����֮�󲢲���ֱ���ͷţ���Ҫ����һ������Ĵ���retired֮������ͷ�����Ĵ���
val regrs1_A = MapTable.read(in_A.regsrc1)
val regrs2_A = MapTable.read(in_A.regsrc2)
val regrs1_B = if(in_A.redges == in_B.regsrc1) old_depend_A else MapTable.read(in_B.regsrc1)
val regrs2_B = if(in_A.redges == in_B.regsrc2) old_depend_A else MapTable.read(in_B.regsrc2)
io.regstate := regstate.avalist
io.regvalues := regvalues.table//���ܹ��Ĵ�����ֵ�����ȥ

//����3�����ύ�����regmapģ��������Ѿ��ύ��ָ����д�����AB����ָ�������ˣ�Ҫ������������Щ�Ĵ���
//�������AB����ָ���Ŀ��Ĵ�����һ���Ļ������ǲ���Ҫ�ύA�������һ����Ҫ�ύ
when (io.cmt_A.redges=/=io.cmt_B/redges){
  cmtable.write(io.cmt_A.Valid,io.cmt_A.redges,io.cmt_A.pregdes)//����ֻҪ����retird��A��Ϣ����
  regvalues.write(io.cmt_A.Valid,io.cmt_A.redges,io.cmt_A.wbdata)//���¶�Ӧ�ļܹ��Ĵ���
}
cmtable.write(io.cmt_B.Valid,io.cmt_B.redges,io.cmt_B.pregdes)//����ֻҪ����retird��A��Ϣ����
regvalues.write(io.cmt_B.Valid,io.cmt_B.redges,io.cmt_B.wbdata)//���¶�Ӧ�ļܹ��Ĵ���

regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.pregdes, 3.U(2.W))
regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.pregdes, 3.U(2.W))//
//�������ȥˢ�����ύ�ļĴ�����״̬Ϊ�Ѿ��ύ
regstate.write(io.cmt_A.Valid && io.cmt_A.finish, io.cmt_A.cmtdes, 0.U(2.W))
regstate.write(io.cmt_B.Valid && io.cmt_B.finish, io.cmt_B.cmtdes, 0.U(2.W))
//Ȼ��������ȥ����Ӧ����һ������������Ĵ�����״̬���Ѿ��ύˢ�µ�free,�Ӷ������cmtָ�������

//����4���ع�����
when(io.rollback){
  //���Ȱ����еļĴ�����ӳ�任���Ѿ��ύ��ӳ��
  for(i <- 0 ro 31){
    MapTable.table(i) := cmtable.table(i)
  }//�����и�������ǻع���ʱ�� ͬʱ�ύ��A��B����ָ���ABˢ�����cmtable�ͻع���when�ǲ��еģ�������Ҫ����������
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
  //���ع�������£���������Ҫ�����µ�ӳ���ϵ���������ӳ���ϵ����д
  //��Ҫ�����������1���Ĵ���ӳ���ϵ�ĸ��� 2������Ĵ���״̬�Ĺ��� 3�����������֮���ָ��
  when (inst_A =/= inst_B){
    MapTable.write (inst_A.Valid,inst_A,redges,regfree_A )
  }
  MapTable.write (inst_B.Valid,inst_B,redges,regfree_B )//�������µ�����Ĵ��������������ϵҪ����
  regstate.write(regfree_A=/=0.U,regfree_A,1.U(2.w))
  regstate.write(regfree_B=/=0.U,regfree_B,1.U(2.w))//ˢϴ�Ĵ�����ռ��״̬��

  //ǰ���ź������ѼĴ�����״̬�����Ĵ�����ռ��״̬���ѵ�ִ�����״̬
  regstate.write(io.fin_A.Valid && io.fin_A.finish, io.fin_A.pregdes, 2.U(2.W))
  regstate.write(io.fin_B.Valid && io.fin_B.finish, io.fin_B.pregdes, 2.U(2.W))
  regstate.write(io.fin_C.Valid && io.fin_C.finish, io.fin_C.pregdes, 2.U(2.W))
  regstate.write(io.fin_D.Valid && io.fin_D.finish, io.fin_D.pregdes, 2.U(2.W))
  regstate.write(io.fin_E.Valid && io.fin_E.finish, io.fin_E.pregdes, 2.U(2.W))

  //���������װ�µ�ָ�����ݿ�
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
  inst_end.reorderNum := reorderNum//������˳��
  inst_end.redges := inst_in.redges
  inst_end.regsrc1 := inst_in.regsrc1
  inst_end.regsrc2 := inst_in.regsrc2
  inst_end.presrc1 := presrc1//���������˸�ӳ��
  inst_end.presrc2 := presrc2//���������˸�ӳ��
  inst_end.predges := predges//���������˸�ӳ��
  inst_end.cmtdes := cmtdes//������������ϵ
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
}//�����������������λ��1
//input : 00101010110
//output: 00100000000
object highbit {
  def apply(data: UInt): UInt = {
    Reverse(lowbit(Reverse(data)))
  }
}