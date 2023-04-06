TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

BASES=pp all main wpp impossible
MLBS=$(BASES:%=%.mlb)
CMS=$(BASES:%=%.cm)
SRCS=$(shell find ../huft/src -name '*.hs')
HUFT=$(TOP)/huft

MAINS=uft

.PHONY: mosml

mosml: $(BIN)/uft
	@: do nothing, and say nothing about doing nothing

.PHONY: clean

clean:
	rm -f *.ui *.uo
	rm -f $(BIN)/uft $(BIN)/uft.opt
	cd ../huft && cabal clean

.PHONY: all

all: $(BIN)/huft
	@: do nothing, and say nothing about doing nothing

msg='$1'

mlton: $(BIN)/uft.opt

$(BIN)/uft.opt: $(MLBS) $(SRCS)
	mlton -output $@ -verbose 1 main.mlb

%.mlb: %.cm
	cm2mlb $^ > $@ || rm -f $@

$(BIN)/uft: $(SRCS) $(CMS)
	$(TOP)/bin/mosml-compile-cm -o $@ main.cm

millet.cm: $(CMS)
	expand-cm main.cm > $@
