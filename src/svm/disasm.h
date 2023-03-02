//// Code for disassembling an instructions, showing values

// Optional, but *very* useful.  I recommend you call
// `svmdebug_value("decode")` at the start of your `vmrun` function,
// and if the result you get back is not NULL, call `idump`
// before decoding each instruction.

#ifndef DISASM_H
#define DISASM_H

#include <stdio.h>

#include "iformat.h"
#include "vmstate.h"

//// simple printer, emits an approximation to assembly code

void printasm(FILE *fp, VMState vm, Instruction i);
  // Works by filling in the `unparsing_template` field
  // in the global instruction table (see itable.h and instructions.h)
  // with these replacements:
  //     iXYZ         replaced with 24-bit signed XYZ field
  //     YZ           replaced with 16-bit unsigned YZ field
  //     X, Y, or Z   replaced with corresponding 8-bit unsigned field
  //     LIT          replaced with literal value at index YZ
  //     GLOBAL       replaced with *name* of global in slot YZ

void idump(FILE *fp, VMState vm, int pc, Instruction I, 
           long int regbase, // A way to communicate where the current 
                             // register window R is pointing.  The number
                             // is printed, but `idump` doesn't compute with it
           Value *RX, Value *RY, Value *RZ);

const char *lastglobalset(struct VMState *vm, uint8_t reg, struct VMFunction *f, Instruction *pc);
  // returns the name of the last global variable that `reg` was set from
  // strictly _before_ `pc`, or if no such name is discoverable, returns NULL

#endif


