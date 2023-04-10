export SHELL = /bin/ksh

TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

.PHONY: all

all: $(BIN)/vscheme $(BIN)/vscheme.opt

$(BIN)/vscheme.opt: vscheme.sml
	mlton -output $@ -verbose 1 $<

$(BIN)/vscheme: vscheme.sml
	mosmlc -o $@ $<

.PHONY: moscow

moscow: $(BIN)/vscheme

.PHONY: mosml

mosml: $(BIN)/vscheme

.PHONY: predef

predef: $(BUILD)/predef-fo.vo $(BUILD)/predef-ho.vo
	@: do nothing and say nothing

$(BUILD)/predef-fo.vo: $(BIN)/vscheme $(BIN)/uft
	$(BIN)/vscheme -predef | grep -vw lambda | $(BIN)/uft fo-vo > $@

$(BUILD)/predef-ho.vo: $(BIN)/vscheme $(BIN)/uft
	$(BIN)/vscheme -predef | $(BIN)/uft ho-vo > $@
