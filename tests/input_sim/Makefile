# This makefile is intended for test compilation and debugging (not for CI or anything like that)
VFLAGS = -g2005-sv -Wall
IVERILOG = iverilog
VVP = vvp
HDLC = cargo run -- 
HIRL_SOURCES = $(wildcard *.hirl)

SVH_SOURCES   = $(patsubst %.hirl, $(OUTPUT_DIR)/%.svh, $(HIRL_SOURCES))
TB_BINARIES   = $(patsubst %.hirl, $(OUTPUT_DIR)/%.vvp, $(HIRL_SOURCES))
VCD_DUMPS     = $(patsubst %.hirl, $(OUTPUT_DIR)/%.vcd, $(HIRL_SOURCES))

OUTPUT_DIR = out

.PHONY: all
all: Makefile $(OUTPUT_DIR) $(TB_BINARIES) 

.PHONY: run
run: Makefile $(OUTPUT_DIR) $(VCD_DUMPS)

.PHONY: clean
clean:
	-rm -rf $(OUTPUT_DIR)

$(OUTPUT_DIR):
	-mkdir $(OUTPUT_DIR)

# I'd prefer $(OUTPUT_DIR)/%.svh rule here but idk if that's portable or not
.PRECIOUS: %.svh
%.svh: ../%.hirl
	$(HDLC) -o $@ $<

# same here
.PRECIOUS: %.vvp
%.vvp: %.svh ../%.sv
	$(IVERILOG) $(VFLAGS) -o $@ $^

%.vcd: %.vvp
	$(VVP) -n $< +DUMP_PATH=$@
