module full_adder(
	input wire a,
	input wire b,
	input wire cin,
	output wire cout,
	output wire q
);
	assign cout = (a & b) | (cin & (a ^ b));
	assign q = a ^ b ^ cin;
endmodule

module div_cell(
	input wire a,
	input wire b,
	input wire s,
	input wire cin,
	output wire cout,
	output wire r
);
	wire fa_out;
	full_adder u_fa(
		.a(a),
		.b(!b),
		.cin(cin),
		.cout(cout),
		.q(fa_out)
	);

	assign r = s ? fa_out : a;
endmodule

module div_comb_stage #(
	parameter WIDTH
)(
	input wire[WIDTH-1:0] a,
	input wire[WIDTH-1:0] b,
	input wire cin,
	output wire cout,
	output wire[WIDTH-1:0] r
);
	genvar i;
	wire[WIDTH-1:0] cin_chain;
	wire[WIDTH-1:0] cout_chain;

	assign cin_chain[0] = cin;
	assign cout = cout_chain[WIDTH - 1];
	assign cin_chain[WIDTH-1:1] = cout_chain[WIDTH-2:0];

	generate for (i = 0; i < WIDTH; i = i + 1) begin
		div_cell u_cell(
			.a(a[i]),
			.b(b[i]),
			.s(cout_chain[WIDTH - 1]),
			.r(r[i]),
			.cin(cin_chain[i]),
			.cout(cout_chain[i])
		);
	end endgenerate

endmodule

module comb_division #(
	parameter A_WIDTH = 16,
	parameter B_WIDTH = 16
)(
	input wire[A_WIDTH-1:0] a,
	input wire[B_WIDTH-1:0] b,
	output wire[A_WIDTH-1:0] q,
	output wire[B_WIDTH-2:0] r
);
	genvar i;
	localparam STAGE_WIDTH = B_WIDTH + 1;
	wire[STAGE_WIDTH-1:0] stage_as[0:A_WIDTH-1];
	wire[STAGE_WIDTH-1:0] stage_b = {{(STAGE_WIDTH - B_WIDTH){1'b0}}, b};
	wire[STAGE_WIDTH-1:0] stage_outputs[0:A_WIDTH-1];

	assign stage_as[0] = {{(STAGE_WIDTH - 1){1'b0}}, a[A_WIDTH - 1]};
	assign r = stage_outputs[A_WIDTH - 1][STAGE_WIDTH - 2:0];

	// Narrow stages
	generate for (i = 0; i < B_WIDTH; i = i + 1) begin
		div_comb_stage #(
			.WIDTH(B_WIDTH)
		)
		stage (
			.a(stage_as[i][STAGE_WIDTH-2:0]),
			.b(stage_b[STAGE_WIDTH-2:0]),
			.cin(1'b1),
			.cout(q[A_WIDTH - 1 - i]),
			.r(stage_outputs[i][STAGE_WIDTH-2:0])
		);

		assign stage_outputs[i][STAGE_WIDTH - 1] = '0;
	end endgenerate

	// Wide stages
	generate for (i = B_WIDTH; i < A_WIDTH; i = i + 1) begin
		div_comb_stage #(
			.WIDTH(B_WIDTH + 1)
		)
		stage (
			.a(stage_as[i]),
			.b(stage_b),
			.cin(1'b1),
			.cout(q[A_WIDTH - 1 - i]),
			.r(stage_outputs[i])
		);
	end endgenerate

	// Stage conns
	generate for (i = 1; i < A_WIDTH; i = i + 1) begin
		assign stage_as[i][0] = a[A_WIDTH - 1 - i];
		assign stage_as[i][STAGE_WIDTH-1:1] = stage_outputs[i - 1][STAGE_WIDTH-2:0];
	end endgenerate

endmodule