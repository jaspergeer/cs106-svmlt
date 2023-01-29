// List of all opcodes, parsers, and unparsers

// You'll develop this list from module 2 onward.  Every time
// you add a new instruction, you'll add an entry here.
// You'll also define the opcode in file opcodes.h,
// and you'll add a case to your `vmrun` function.

#include "iformat.h"
#include "name.h"
#include "itable.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

instruction_info instructions[] = {
  { "halt", Halt, parseR0, "halt" },
  { "print", Print, parseR1, "print rX" },
  { "println", Println, parseR1, "println rX" },
  { "loadliteral", LoadLiteral, parseR1LIT, "rX := LIT" },
  { "check", Check, parseR1LIT, "check LIT, rX" },
  { "expect", Expect, parseR1LIT, "expect LIT, rX" },
};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code) {
  (void) code;
  return 0; // change this for your SVM
}

