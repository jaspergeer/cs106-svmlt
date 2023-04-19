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

.PHONY: clean

clean:
	rm -f *.ui *.uo
	rm -f $(BIN)/uft $(BIN)/uft.opt
	cd ../huft && cabal clean

.PHONY: all

all: $(BIN)/huft

msg='$1'

$(BIN)/uft: $(SRCS)
	cd ../huft && cabal build huft && ln -sf `cabal list-bin huft` ../../bin/uft

millet.cm: $(CMS)
	expand-cm main.cm > $@