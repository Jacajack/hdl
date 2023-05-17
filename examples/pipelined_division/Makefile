VC = iverilog
VRUN = vvp
VFLAGS = -Wall -Winfloop -Wno-timescale -g2005-sv

all: pipelined_division_tb

run: pipelined_division_tb
	$(VRUN) pipelined_division_tb

pipelined_division_tb: tb.sv ref.sv
	$(VC) $(VFLAGS) -o $@ $^