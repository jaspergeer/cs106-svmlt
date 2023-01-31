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

  // Printing
  { "print", Print, parseR1, "print rX" },
  { "println", Println, parseR1, "println rX" },
  { "printu", Printu, parseR1, "printu rX" },

  // Dynamic Loading
  { "popen", PipeOpen, parseR1LIT, "open pipe from LIT" },
  { "dload", DynLoad, parseR1, "load from rX" },

  // Branching
  { "cskip", CondSkip, parseR1, "if rX then skip" },
  { "jump", Jump, parseR0I24, "jump to iXYZ" },

  // Load/Store
  { "loadliteral", LoadLiteral, parseR1LIT, "rX := LIT" },
  { "getglobal", GetGLobal, parseR1GLO, "rX := GLOBAL" },
  { "setglobal", SetGlobal, parseR1GLO, "GLOBAL := rX" },

  // Check/Expect
  { "check", Check, parseR1LIT, "check LIT, rX" },
  { "expect", Expect, parseR1LIT, "expect LIT, rX" },

  // Arithmetic
  { "add", Add, parseR3, "rX := rY + rZ" },
  { "sub", Add, parseR3, "rX := rY - rZ" },
  { "mul", Add, parseR3, "rX := rY * rZ" },
  { "div", Add, parseR3, "rX := rY / rZ" },
  { "mod", Add, parseR3, "rX := rY % rZ" },

  // Boolean
  { "truth", Truth, parseR2, "rX := truthiness rY" },
  { "not", Not, parseR2, "rX := !rY"},
  { "and", And, parseR3, "rX := rY && rZ" },
  { "or", Or, parseR3, "rX := rY || rZ" },
  { "xor", Xor, parseR3, "rX := rY ^ rZ" },

  // Comparison
  { "cmp", Cmp, parseR3, "rX := rY == rZ"},
  { "gt", Gt, parseR3, "rX := rY > rZ" },
  { "lt", Lt, parseR3, "rX := rY < rZ" },
  { "ge", Ge, parseR3, "rX := rY >= rZ" },
  { "le", Le, parseR3, "rX := rY <= rZ" },

  // S-Expressions
  { "cons", Cons, parseR3, "cons rX, rY, rZ" },
  { "car", Car, parseR2, "car rX, rY" },
  { "cdr", Cdr, parseR2, "cdr rX, rY" }
};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code) {
  return code == GetGLobal;
}

