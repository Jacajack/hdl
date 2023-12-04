`include "tb_common.svh"

module width_sem_test_tb;
	reg[7:0] a, b;
	wire[15:0] c;

	width_sem_test_dut dut(
		.a(a),
		.b(b),
		.c(c)
	);

	initial begin
		`START_DUMP(width_sem_test_tb);

		a = 0;
		b = 0; 
		#1;

		for (int i = 0; i < 256; i++) begin
			a = i;
			for (int j = 0; j < 256; j++) begin
				b = j;
				#1;
				`ASSERT(c == i + j);
			end
		end
	end


endmodule
