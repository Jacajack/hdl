`include "tb_common.svh"

module stupid_multiply_tb;
	reg clk;
	reg nreset;
	reg input_valid;
	wire input_ready;
	reg output_ready;
	wire output_valid;
	reg[7:0] a, b;
	reg[15:0] c;

	stupid_multiply dut(
		.clk(clk),
		.nreset(nreset),
		.input_valid(input_valid),
		.input_ready(input_ready),
		.output_valid(output_valid),
		.output_ready(output_ready),
		.a(a),
		.b(b),
		.c(c)
	);

	task wait_cycles(int n);
		for (int i = 0; i < n; i++) begin
			clk = '1;
			#1;
			clk = '0;
			#1;
		end
	endtask

	task check_multiply(int a_val, int b_val);
		input_valid = '0;
		output_ready = '0;
		a = a_val;
		b = b_val;

		wait_cycles(10);
		input_valid = 1;
		wait_cycles(1);
		input_valid = 0;

		while (!output_valid) begin
			wait_cycles(1);
		end

		`ASSERT(output_valid == 1);
		`ASSERT(c == a * b);
		output_ready = 1;
		wait_cycles(1);
		output_ready = 0;
	endtask

	initial begin
		`START_DUMP(stupid_multiply_tb)

		clk = '0;
		nreset = '0;
		input_valid = '0;
		output_ready = '0;
		a = 17;
		b = 34;
		
		#1;
		nreset = '1;

		for (int i = 0; i < 1000; i++) begin
			check_multiply($urandom(), $urandom());
		end

		$finish;
	end

endmodule
