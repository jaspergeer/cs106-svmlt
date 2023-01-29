TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

UNITS = testfuns vmheap vmrun vmstate vmrun vmstring value iformat \
        printbuf print check-expect testfuns vmerror
MAINS = svm-test


# Module 2 adds the ability to load virtual object code, plus the svm binary

UNITS += loader iparsers tokens name itable instructions svmdebug disasm vtable
MAINS += svm svm-loader-instructions


OBJ := $(UNITS:%=$(BUILD)/%.o)
MOBJ := $(MAINS:%=$(BUILD)/%.o)

EXE := $(MAINS:%=$(BIN)/%)

DEPS := $(OBJ:.o=.d)

CPPFLAGS += -MMD -MP

CFLAGS = -std=c11 -pedantic -Wall -Wextra -Werror -g -I/usr/include

.PHONY: all obj clean
all: $(EXE)
obj: $(OBJ) $(MOBJ)

clean:
	rm -f $(OBJ) $(MOBJ) $(DEPS) $(EXE)


.PHONY: show
show: 
	echo $(OBJ) $(MOBJ)

.PHONY: svm-test

svm-test: $(BIN)/svm-test

$(BIN)/svm-test: $(OBJ) $(BUILD)/svm-test.o
	gcc $(CFLAGS) -o $@ $^ -lm


.PHONY: svm

svm: $(BIN)/svm

$(BIN)/svm: $(OBJ) $(BUILD)/svm.o
	gcc $(CFLAGS) -o $@ $^ -lm

$(BIN)/svm-loader-instructions: $(OBJ) $(BUILD)/svm-loader-instructions.o
	gcc $(CFLAGS) -o $@ $^ -lm

all: $(BUILD)/model-state.o
all: $(BUILD)/model-run.o

$(BUILD)/model-state.o: model/vmstate.c model/vmstate.h
	gcc -c $(CFLAGS) -Imodel -I. -o $@ model/vmstate.c
$(BUILD)/model-run.o: model/vmrun.c model/vmstate.h
	gcc -c $(CFLAGS) -Imodel -I. -o $@ model/vmrun.c





$(BUILD)/%.o: %.c
	gcc $(CFLAGS) -MMD -MF $(BUILD)/$*.d -o $@ -c -Wall -Werror -std=c11 $<


-include $(DEPS)
