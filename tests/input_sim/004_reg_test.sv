`include "tb_common.svh"

module reg_test_tb;
	reg clk;
	reg en;
	reg nreset;
	reg[7:0] next;
	wire[7:0] data;

	reg_wrapper #(
		.WIDTH(8)
	) dut (
		.clk(clk),
		.en(en),
		.nreset(nreset),
		.next(next),
		.data(data)
	);

	task clk_tick();
		clk = '1;
		#1;
		clk = '0;
		#1;
	endtask

	initial begin
		`START_DUMP(reg_test_tb);
		clk = '0;
		nreset = '0;
		en = '0;
		next = '0;
		#1;

		// Return from reset
		nreset = '1;
		#1;
		`ASSERT(data == 0);

		// Few cycles of nothing
		clk_tick();
		clk_tick();
		clk_tick();
		`ASSERT(data == 0);

		// Set next input but don't enable EN
		next = 42;
		clk_tick();
		`ASSERT(data == 0);

		// Enable EN
		en = '1;
		clk_tick();
		en = '0;
		`ASSERT(data == 42);

		// Async reset
		nreset = '0;
		#1;
		`ASSERT(data == 0);
		nreset = '1;
		#1;
		`ASSERT(data == 0);

		// Write data when nreset is down
		nreset = '0;
		next = 17;
		en = '1;
		clk_tick();
		`ASSERT(data == 0);
		clk_tick();
		`ASSERT(data == 0);
		nreset = '1;
		#1;
		`ASSERT(data == 0); // Not yet!
		clk_tick();
		`ASSERT(data == 17); // Now!

		// Make sure the data is actually updated on rising edge
		next = 42;
		en = '1;
		clk = '1;
		`ASSERT(data == 17);
		#1;
		`ASSERT(data == 42);
		next = 11;
		clk = '0;
		#1;
		`ASSERT(data == 42);

		// Nreset again
		nreset = '0; 
		#1;
		`ASSERT(data == 0);
	end


endmodule
