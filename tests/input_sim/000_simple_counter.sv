// TODO move this into a shared TB lib
`define ASSERT(cond) assert(cond) else $fatal(1)

task start_dump();
	string vcd_path;
	$value$plusargs("DUMP_PATH=%s", vcd_path);
	$display("DUMP_PATH=%s", vcd_path);
	$dumpfile(vcd_path);
	$dumpvars(0, simple_counter_tb);
endtask


module simple_counter_ref(
	input wire clk,
	input wire en,
	input wire nreset,
	output wire[15:0] data
);

reg[15:0] cnt = '0;
always @(posedge clk)
	if (!nreset)
		cnt <= '0;
	else if (en)
		cnt <= cnt + 1;
	else
		cnt <= cnt;

assign data = cnt;

endmodule


module simple_counter_tb;
	reg clk;
	reg en;
	reg nreset;
	wire[15:0] ref_data;
	wire[15:0] dut_data;

	simple_counter_ref ref_cnt(
		.clk(clk),
		.en(en),
		.nreset(nreset),
		.data(ref_data)
	);

	simple_counter dut_cnt(
		.clk(clk),
		.enable(en),
		.nreset(nreset),
		.data(dut_data)
	);

	initial begin
		start_dump();

		clk = '0;
		en = '0;
		nreset = '0;
		
		$display("hello cruel world");
		#10;
		nreset = '1;


		for (int i = 0; i < 65536 * 2; i++) begin
			if (i > 14)
				en = '1;
			else
				en = '0;
			
			clk = '1;
			#1;
			clk = '0;
			#1;

			`ASSERT(dut_data == ref_data);
		end
		
		$finish;
	end

endmodule