`timescale 1ns/1ps

// TODO move this into a shared TB lib
`define ASSERT(cond) assert(cond) else $fatal(1)

task start_dump();
	string vcd_path;
	$value$plusargs("DUMP_PATH=%s", vcd_path);
	$display("DUMP_PATH=%s", vcd_path);
	$dumpfile(vcd_path);
	$dumpvars(0, comb_div_tb);
endtask


module ref_full_adder(
	input wire a,
	input wire b,
	input wire cin,
	output wire cout,
	output wire q
);
	assign cout = (a & b) | (cin & (a ^ b));
	assign q = a ^ b ^ cin;
endmodule

module ref_div_cell(
	input wire a,
	input wire b,
	input wire s,
	input wire cin,
	output wire cout,
	output wire r
);
	wire fa_out;
	ref_full_adder u_fa(
		.a(a),
		.b(!b),
		.cin(cin),
		.cout(cout),
		.q(fa_out)
	);

	assign r = s ? fa_out : a;
endmodule

module comb_div_tb;
	reg[3:0] inputs;
	wire[1:0] ref_outputs;
	wire[1:0] dut_outputs;

	ref_div_cell ref_cell(
		.a(inputs[3]),
		.b(inputs[2]),
		.s(inputs[1]),
		.cin(inputs[0]),
		.cout(ref_outputs[1]),
		.r(ref_outputs[0])
	);

	div_cell dut_cell(
		.a(inputs[3]),
		.b(inputs[2]),
		.s(inputs[1]),
		.cin(inputs[0]),
		.cout(dut_outputs[1]),
		.r(dut_outputs[0])
	);

	initial begin
		start_dump();

		inputs = 0;
		#1;

		for (int i = 0; i < 16; i++) begin
			inputs = i;
			#1;
			`ASSERT(ref_outputs == dut_outputs);
		end
	end


endmodule