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

.PHONY: all

all: $(BIN)/huft

msg='$1'

$(BIN)/uft: $(SRCS)
	cd ../huft && cabal build huft
	echo "#!/bin/sh" > $@
	echo -e -n 'lang=\044' >> $@
	echo -e -n '1\n' >> $@
	echo -e -n 'file=\044' >> $@
	echo -e -n '2\n' >> $@
	(cd ../huft && cabal list-bin huft) > temp
	echo -e -n $(shell cat temp) '\044lang \044file' >> $@
	(chmod u+x $@)
	rm temp

millet.cm: $(CMS)
	expand-cm main.cm > $@
