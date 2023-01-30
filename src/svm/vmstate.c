// Memory management and literal addition for VMState

// You'll complete this file as part of module 1


#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "value.h"
#include "vmerror.h"

void freestatep(VMState* sp) {
  assert(sp && *sp);

  LPool_free(&(*sp)->literals);
  VMState vm = *sp;
  free(vm);
  //(void)vm; // suppress compiler warnings
  //assert(0); // must free all memory associated with `vm`
}

VMState newstate(void) {
  // allocate, initialize, and return a new state
  VMState vm = malloc(sizeof(struct VMState));
  vm->pc = 0;
  vm->num_globals = 0;
  vm->literals = LPool_new();
  for (int i = 0; i < 256; ++i) {
    vm->registers[i] = nilValue;
  }
  return vm;
}

int literal_slot(VMState state, Value literal) {
  return LPool_put(state->literals, literal);
}

Value literal_value(VMState state, unsigned index) {
  return LPool_get(state->literals, index);
}

int literal_count(VMState state) {
  return LPool_nlits(state->literals);
}

int global_slot(VMState state, Value global) {
  if (state->num_globals == GLOBALS_SIZE)
    runerror(state, "globals limit reached");

  state->globals[state->num_globals] = global;
  return state->num_globals++;
}

const char *global_name(VMState state, unsigned index) {
    (void) state;
    (void) index;
    return "x";
}
