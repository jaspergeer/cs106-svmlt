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
  { "zero", Unimp, parseR1, "rX := 0" },

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
  { "+", Add, parseR3, "rX := rY + rZ" },
  { "-", Sub, parseR3, "rX := rY - rZ" },
  { "*", Mul, parseR3, "rX := rY * rZ" },
  { "/", Div, parseR3, "rX := rY / rZ" },
  { "mod", Mod, parseR3, "rX := rY mod rZ" },

  // Boolean
  { "truth", Truth, parseR2, "rX := truth rY" },
  { "not", Not, parseR2, "rX := not rY"},
  { "and", And, parseR3, "rX := rY and rZ" },
  { "or", Or, parseR3, "rX := rY or rZ" },
  { "xor", Xor, parseR3, "rX := rY xor rZ" },

  // Comparison
  { "n=", Cmp, parseR3, "rX := rY n= rZ"},
  { "s=", Unimp, parseR3, "rX := rY s= rZ"},
  { ">", Gt, parseR3, "rX := rY > rZ" },
  { "<", Lt, parseR3, "rX := rY < rZ" },
  { ">=", Ge, parseR3, "rX := rY >= rZ" },
  { "<=", Le, parseR3, "rX := rY <= rZ" },

  // S-Expressions
  { "cons", Cons, parseR3, "cons rX, rY, rZ" },
  { "car", Car, parseR2, "car rX, rY" },
  { "cdr", Cdr, parseR2, "cdr rX, rY" }
};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code) {
  return code == GetGLobal;
}

