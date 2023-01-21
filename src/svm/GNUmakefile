TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

UNITS = testfuns vmheap vmrun vmstate vmrun vmstring value iformat \
        printbuf print check-expect testfuns vmerror
MAINS = svm-test


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




$(BUILD)/%.o: %.c
	gcc $(CFLAGS) -MMD -MF $(BUILD)/$*.d -o $@ -c -Wall -Werror -std=c11 $<


-include $(DEPS)
