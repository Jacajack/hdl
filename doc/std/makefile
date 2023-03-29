INPUT = standard.md
OUTPUT = $(patsubst %.md,%.pdf,$(INPUT))
HEADER = header.tex

$(OUTPUT): $(INPUT) $(HEADER) makefile
	pandoc --listings --pdf-engine=lualatex $< -V urlcolor=blue --highlight-style tango.theme -H $(HEADER) -o $@
