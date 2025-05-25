module BindsTo_2_mem(
  input         clock,
  input         reset,
  input         io_reset,
  input  [63:0] io_if_mem_instAddr,
  input  [63:0] io_ex_mem_dataAddr,
  input         io_ex_mem_writeEn,
  input  [31:0] io_ex_mem_writeData,
  input  [2:0]  io_ex_mem_func3,
  output [31:0] io_mem_id_inst_0,
  output [31:0] io_mem_id_inst_1,
  output [31:0] io_mem_lsu_data
);

initial begin
  $readmemh("G:\\testdata\\drystone_2.data", mem.memInside_2);
end
                      endmodule

bind mem BindsTo_2_mem BindsTo_2_mem_Inst(.*);