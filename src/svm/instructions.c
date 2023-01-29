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
//   { "println", Println, parseR1, "println rX" },
//   { "printu", Printu, parseR1, "printu rX" },
  { "loadliteral", LoadLiteral, parseR1LIT, "rX := LIT" },
  { "cmov", Expect, parseR3, "if rY then rX := rZ" },
  { "jmp", Jump, parseR0I24, "jmp iXYZ" },

  // Load/Store
  { "loadlit", LoadLiteral, parseR1LIT, "rX := G[LIT]" },

  // Check-Expect
  { "check", Check, parseR1LIT, "check LIT, rX" },
  { "expect", Expect, parseR1LIT, "expect LIT, rX" },

  // Arithmetic
  { "add", Add, parseR3, "rX := rY + rZ" },
  { "sub", Add, parseR3, "rX := rY - rZ" },
  { "mul", Add, parseR3, "rX := rY * rZ" },
  { "div", Add, parseR3, "rX := rY / rZ" },

  // Boolean Logic
  { "truth", Truth, parseR3, "rX := truthiness rY" },
  { "and", And, parseR3, "rX := rY && rZ" },
  { "or", Or, parseR3, "rX := rY || rZ" },
  { "xor", Xor, parseR3, "rX := rY ^ rZ" },

  // Comparison
  { "cmp", Cmp, parseR3, "rX := rY == rZ"},
  { "gt", Gt, parseR3, "rX := rY > rZ" },
  { "lt", Lt, parseR3, "rX := rY < rZ" },
  { "ge", Ge, parseR3, "rX := rY >= rZ" },
  { "le", Le, parseR3, "rX := rY <= rZ" }
};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code) {
  (void) code;
  return 0; // change this for your SVM
}

