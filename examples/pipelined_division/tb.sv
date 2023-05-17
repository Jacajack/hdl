module tb;

localparam A_WIDTH = 16;
localparam B_WIDTH = 16;

wire[A_WIDTH-1:0] a = 622;
wire[B_WIDTH-1:0] b = 3;
wire[A_WIDTH-1:0] q;
wire[B_WIDTH-2:0] r;

comb_division #(
	.A_WIDTH(A_WIDTH),
	.B_WIDTH(B_WIDTH)
)
u_comb_division(
	.a(a),
	.b(b),
	.q(q),
	.r(r)
);

initial begin
	#10;
	$display("A = %d", a);
	$display("B = %d", b);
	$display("A / B = %d", q);
	$display("A %% B = %d", r);
	$dumpfile("dump.vcd");
	$dumpvars(0, tb);
	$finish;
end

endmodule