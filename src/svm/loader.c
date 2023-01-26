// The VM loader: read virtual object code and update VM state

// This is the focus of module 2.  In future modules, we'll continue
// to use it, but we won't revise it except for an optional depth goal.
//
// You'll write the core of the loader: function `loadfun`.  This
// function loads a single VM function from a .vo file.  It is called
// by `loadmodule`, which load a single module, and which I provide.
// (A module is just a function that takes no parameters).  I also
// provide `loadmodules`, which loads a list of modules.  Finally, I
// provide `parse_instruction`, which uses the parsing table in file
// instructions.c to parse each single instruction.



#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "loader.h"
#include "itable.h"
#include "tokens.h"
#include "svmdebug.h"
#include "vmstate.h"
#include "vmheap.h"
#include "vmsizes.h"
#include "vmstring.h"
#include "value.h"

//// Variables and utility functions local to this module

static bool saw_a_bad_opcode = false;  

static Instruction
  parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp);
  // Parse `operands` according to the parser for the given `opcode`.
  // Return the parsed instruction.  If the instruction mentions any
  // register, update `*maxregp` to be at least as large as any
  // mentioned register (and no smaller than it was).
  //
  // If the opcode is not recognized, set `saw_a_bad_opcode` and return 0.

static char *buffer; // for use by getline(3), shared by functions
size_t bufsize;      // `loadmodule` and `get_instruction`



//// The functions you will write

static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *input);
  // Allocates and returns a new function using `vmalloc`, with all fields
  // correctly set:
  //   - The arity is given by parameter `arity`.
  //   - The number of instructions is given by parameter `count`.
  //   - The instructions themselves are read from `input`.
  //   - The `nregs` field must be set to one more than the largest
  //     register mentioned in any instruction.

static Instruction get_instruction(VMState vm, FILE *vofile, unsigned *maxregp);
  // Consumes one <instruction> nonterminal from `vofile`,
  // encodes the resulting instruction `i`, and returns it.
  // Also has the following side effects:
  //    - On return, every literal mentioned in the instruction `i`
  //      is present in vm's literal pool.
  //    - For each register X, Y, or Z mentioned in `i`, `*maxregp`
  //      is set to the larger itself and X, Y, or Z.
  // May overwrite `buffer` and `bufsize` by calling `getline`.

static Instruction get_instruction(VMState vm, FILE *vofile, unsigned *maxregp) {
  (void) vm; (void) vofile; (void) maxregp; // replace with real code  
  (void) parse_instruction; // replace with real code
  assert(0);
}

static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *vofile) {
  (void) vm; (void) arity; (void) count; (void) vofile; // replace with real code
  (void) get_instruction; // replace with real code
  assert(0);
}


//// module loading, with helper function

static bool has_input(FILE *fd) {
  int c = getc(fd);
  if (c == EOF) {
    return false;
  } else {
    ungetc(c, fd);
    return true;
  }
}


struct VMFunction *loadmodule(VMState vm, FILE *vofile) {
  // precondition: `vofile` has input remaining

  static Name dotloadname, modulename;
  if (dotloadname == NULL) {
    dotloadname = strtoname(".load");
    modulename  = strtoname("module");
  }

  if (!has_input(vofile))
    return NULL;

  // read a line from `vofile` and tokenize it
  if (getline(&buffer, &bufsize, vofile) < 0) {
    // end of file, spec calls for NULL to be returned
    assert(false);
  }
  Tokens alltokens = tokens(buffer);
  Tokens tokens_left = alltokens;

  // parse the tokens; expecting ".load module <count>"
  Name n;
  n = tokens_get_name(&tokens_left, buffer); // removes token from tokens_left
  assert(n == dotloadname);
  n = tokens_get_name(&tokens_left, buffer);
  assert(n == modulename);
  uint32_t count = tokens_get_int(&tokens_left, buffer);
  assert(tokens_left == NULL); // that must be the last token

  struct VMFunction *module = loadfun(vm, 0, count, vofile); // read the remaining instructions

  if (buffer) {
    free(buffer);
    buffer = NULL;
  }
  free_tokens(&alltokens);

  return module;
}


///// utility functions

static Instruction
parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp) {
  instruction_info *info = itable_entry(opcode);
  if (info) {
    return info->parser(vm, info->opcode, operands, maxregp);
  } else {
    fprintf(stderr, "No opcode for %s.\n", nametostr(opcode));
    saw_a_bad_opcode = true;
    return 0;
  }
}
