TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

BASES=pp all main wpp impossible
MLBS=$(BASES:%=%.mlb)
CMS=$(BASES:%=%.cm)
SRCS=$(shell find ../uft/src -name '*.hs')
UFT=$(TOP)/uft

MAINS=uft

.PHONY: mosml

mosml: $(BIN)/uft

.PHONY: clean

clean:
	rm -f *.ui *.uo
	rm -f $(BIN)/uft $(BIN)/uft.opt
	cabal clean

.PHONY: all

all: $(BIN)/uft

msg='$1'

$(BIN)/uft: $(SRCS)
	cabal build uft && ln -sf `cabal list-bin uft` ../../bin/uft

millet.cm: $(CMS)
	expand-cm main.cm > $@
