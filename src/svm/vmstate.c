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
  vm->stack_ptr = vm->call_stack - 1;
  vm->awaiting_expect = nilValue;
  for (int i = 0; i < 256; ++i) {
    vm->registers[i] = nilValue;
  }
  vm->reg0 = vm->registers;
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

#include "print.h"

int global_slot(VMState state, Value global) {
    // (void) AsCString
    Name name = strtoname(AS_CSTRING(state, global));
    int slot;
    for (slot = 0; slot < state->num_globals; ++slot) {
      if (state->global_names[slot] == name)
        return slot;
    }
    slot = state->num_globals++;
    assert(slot < GLOBALS_SIZE);
    state->global_names[slot] = name;
    state->globals[slot] = nilValue;
    return slot;
}

const char* global_name(VMState state, unsigned index) {
  return nametostr(state->global_names[index]);
}

void initialize_global(VMState vm, Value name, Value v) {
  (void) vm; (void) name; (void) v; // replace with real code
  assert(0);
}
