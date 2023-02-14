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
  { "zero", Zero, parseR1, "zero rX" },
  { "hash", Hash, parseR2, "hash rX rY"},
  { "copy", Copy, parseR2, "copy rX rY"},
  { "err", Err, parseR1, "err rX" },

  // Printing
  { "print", Print, parseR1, "print rX" },
  { "println", Println, parseR1, "println rX" },
  { "printu", Printu, parseR1, "printu rX" },

  // Dynamic Loading
  { "popen", PipeOpen, parseR1LIT, "popen rX LIT" },
  { "dload", DynLoad, parseR1, "dload rX" },

  // Branching
  { "cskip", CondSkip, parseR1, "cskip rX" },
  { "jump", Jump, parseR0I24, "jump iXYZ" },

  // Load/Store
  { "loadliteral", LoadLiteral, parseR1LIT, "loadliteral rX LIT" },
  { "getglobal", GetGlobal, parseR1GLO, "getglobal rX GLOBAL" },
  { "setglobal", SetGlobal, parseR1GLO, "setglobal rX GLOBAL" },

  // Check/Expect
  { "check", Check, parseR1LIT, "check rX LIT" },
  { "expect", Expect, parseR1LIT, "expect rX LIT" },

  // Arithmetic
  { "+", Add, parseR3, "+ rX rY rZ" },
  { "-", Sub, parseR3, "- rX rY rZ" },
  { "*", Mul, parseR3, "* rX rY rZ" },
  { "/", Div, parseR3, "/ rX rY rZ" },
  { "mod", Mod, parseR3, "mod rX rY rZ" },
  { "idiv", Idiv, parseR3, "idiv rX rY rZ"},

  // Boolean
  { "truth", Truth, parseR2, "truth rX rY" },
  { "not", Not, parseR2, "not rX rY"},
  { "and", And, parseR3, "and rX rY rZ" },
  { "or", Or, parseR3, "or rX rY rZ" },
  { "xor", Xor, parseR3, "xor rX rY rZ" },

  // Comparison
  { "n=", Cmp, parseR3, "n= rX rY rZ"},
  { "s=", Unimp, parseR3, "s= rX rY rZ"},
  { ">", Gt, parseR3, "> rX rY rZ" },
  { "<", Lt, parseR3, "< rX rY rZ" },
  { ">=", Ge, parseR3, ">= rX rY rZ" },
  { "<=", Le, parseR3, "<= rX rY rZ" },

  // S-Expressions
  { "cons", Cons, parseR3, "cons rX rY rZ" },
  { "car", Car, parseR2, "car rX rY" },
  { "cdr", Cdr, parseR2, "cdr rX rY" },

  // type predicates
  { "function?", IsFunc, parseR2, "function? rX rY" },
  { "pair?", IsPair, parseR2, "pair? rX rY" },
  { "symbol?", IsSym, parseR2, "symbol? rX rY" },
  { "number?", IsNum, parseR2, "number? rX rY" },
  { "boolean?", IsBool, parseR2, "boolean? rX rY" },
  { "null?", IsNull, parseR2, "null? rX rY" },
  { "nil?", IsNil, parseR2, "nil? rX rY"}
  };

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code) {
  return code == GetGlobal;
}

