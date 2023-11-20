`include "tb_common.svh"

module fancy_counter_ref(
	input wire clk,
	input wire en,
	input wire nreset,
	output wire[15:0] data,
	output wire[15:0] fancy_data
);

reg[15:0] cnt = '0;
always @(posedge clk)
	if (!nreset)
		cnt <= '0;
	else if (en)
		cnt <= cnt + 1;
	else
		cnt <= cnt;

reg[15:0] delayed = '0;
always @(posedge clk)
	if (!nreset)
		delayed <= '0;
	else if (en)
		delayed <= cnt;
	else
		delayed <= delayed;

assign data = cnt;
assign fancy_data =
	(data == 0)  ? 17 :
	(data == 1)  ? 1287 :
	(data == 17) ? 2137 :
	(delayed ^ cnt) + cnt;

endmodule


module fancy_counter_tb;
	reg clk;
	reg en;
	reg nreset;
	wire[15:0] ref_data;
	wire[15:0] dut_data;
	wire[15:0] ref_fancy_data;
	wire[15:0] dut_fancy_data;

	fancy_counter_ref ref_cnt(
		.clk(clk),
		.en(en),
		.nreset(nreset),
		.data(ref_data),
		.fancy_data(ref_fancy_data)
	);

	fancy_counter dut_cnt(
		.clk(clk),
		.en(en),
		.nreset(nreset),
		.data(dut_data),
		.fancy_data(dut_fancy_data)
	);

	initial begin
		`START_DUMP(fancy_counter_tb);

		clk = '0;
		en = '0;
		nreset = '0;
		
		$display("hello cruel world");
		#10;
		nreset = '1;


		for (int i = 0; i < 65536 * 2; i++) begin
			en = i > 14; 
			nreset = !(i > 65536 + 1078 && i < 65536 + 1098);

			clk = '1;
			#1;
			clk = '0;
			#1;

			`ASSERT(dut_data == ref_data);
			`ASSERT(dut_fancy_data == ref_fancy_data);
		end
		
		$finish;
	end

endmodule
