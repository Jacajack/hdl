`include "tb_common.svh"

typedef struct packed {
	logic[3:0] id;
	logic[15:0] addr;
	logic[8:0] len;
	logic[1:0] size;

	int delay;
	logic[8:0] transferred;
	int age;
} axi_request;

module simple_dma_tb;
	
	reg[7:0] mem_data[65536];
	axi_request current_request;
	bit current_request_valid;


	reg clk;
	reg nreset;

	wire axi_arvalid;
	reg  axi_arready;
	wire[3:0] axi_arid;
	wire[15:0] axi_araddr;
	wire[7:0] axi_arlen;
	wire[1:0] axi_arsize;
	wire[1:0] axi_arburst;
	wire[1:0] axi_arlock;
	wire[3:0] axi_arcache;
	wire[2:0] axi_arprot;
	wire[3:0] axi_arqos;

	reg axi_rvalid;
	wire axi_rready;
	reg[3:0] axi_rid;
	reg[7:0] axi_rdata;
	reg[1:0] axi_rresp;
	reg axi_rlast;

	reg[15:0] dma_addr;
	reg dma_addr_valid;
	wire[7:0] dma_data;
	wire dma_data_valid;
	reg[7:0] exp_data;

	int cycles;


	axi_dma_addr16_data8_id4 dut_dma(
		.clk(clk),
		.nreset(nreset),
		
		.axi_arvalid(axi_arvalid),
		.axi_arready(axi_arready),
		.axi_arid(axi_arid),
		.axi_araddr(axi_araddr),
		.axi_arlen(axi_arlen),
		.axi_arsize(axi_arsize),
		.axi_arburst(axi_arburst),
		.axi_arlock(axi_arlock),
		.axi_arcache(axi_arcache),
		.axi_arprot(axi_arprot),
		.axi_arqos(axi_arqos),

		.axi_rvalid(axi_rvalid),
		.axi_rready(axi_rready),
		.axi_rid(axi_rid),
		.axi_rdata(axi_rdata),
		.axi_rresp(axi_rresp),
		.axi_rlast(axi_rlast),

		.dma_addr(dma_addr),
		.dma_addr_valid(dma_addr_valid),
		.dma_data(dma_data),
		.dma_data_valid(dma_data_valid)
	);
	
	always @(posedge clk) begin 
		// Accept address request
		if (axi_arready && axi_arvalid) begin
			current_request_valid = 1;
			current_request.id = axi_arid;
			current_request.addr = axi_araddr;
			current_request.len = axi_arlen + 1;
			current_request.size = axi_arsize;
			current_request.transferred = 0;
			current_request.age = 0;
			current_request.delay = $urandom_range(67);
			axi_arready <= 0;
			//$display("addr request accepted");
		end else begin
			axi_arready <= !current_request_valid;
		end

		if (current_request_valid)
			++current_request.age;


		if (current_request_valid && axi_rready && axi_rvalid) begin
			//$display("data transfer");
			current_request.transferred++;
			if (current_request.transferred == current_request.len)
				current_request_valid = 0;
		end

		axi_rid <= current_request.id;
			axi_rdata <= mem_data[current_request.addr + current_request.transferred];
			axi_rresp <= 0;
		axi_rlast <= current_request.transferred == current_request.len;
		axi_rvalid <= current_request_valid && current_request.age >= current_request.delay && (current_request.transferred < current_request.len);

		if (dma_data_valid)
			`ASSERT(dma_data == exp_data);
	end



	initial begin
		`START_DUMP(simple_dma_tb);

		// mem init
		for (int i = 0; i < 65536; i++) begin
			mem_data[i] = (i << 4) ^ 'hFA + 674 * i + 111;
		end

		clk = 0;
		nreset = 0;
		axi_arready = 0;
		axi_rvalid = 0;
		dma_addr = 0;
		dma_addr_valid = 0;

		#1;
		nreset = 1;
		#1;

		
		for (int addr = 0; addr < 100; addr++) begin
			dma_addr = addr;
			dma_addr_valid = 1;
			exp_data = mem_data[addr];

			#1;
			clk = 1;
			#1;
			clk = 0;

			cycles = 0;
			while (!dma_data_valid) begin
				#1;
				clk = 1;
				#1;
				clk = 0;
				
				cycles++;
				`ASSERT(cycles < 1000);
			end

			//`ASSERT(dma_data == exp_data);

		end

		for (int addr = 4574; addr < 4578; addr++) begin
			dma_addr = addr;
			dma_addr_valid = 1;

			#1;
			clk = 1;
			#1;
			clk = 0;

			dma_addr = addr + 1;
			exp_data = mem_data[addr + 1];

			cycles = 0;
			while (!dma_data_valid) begin
				#1;
				clk = 1;
				#1;
				clk = 0;
				
				cycles++;
				`ASSERT(cycles < 1000);
			end

			//`ASSERT(dma_data == exp_data);

		end
		
	end
endmodule
