// Memory management and literal addition for VMState

// You'll complete this file as part of module 1


#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "value.h"

void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    free(vm->registers);
    free(vm->literals);
    free(vm);
    //(void)vm; // suppress compiler warnings
    //assert(0); // must free all memory associated with `vm`
}

VMState newstate(void) {
    // allocate, initialize, and return a new state
    VMState vm = malloc(sizeof(struct VMState));
    vm -> registers = calloc(256, sizeof(struct Value));
    vm -> literals = calloc(LITERAL_INIT_LENGTH, sizeof(struct Value));
    vm -> curLiteralSize = 0;
    vm -> maxLiteralSize = LITERAL_INIT_LENGTH;
    vm -> code = NULL;
    vm -> pc = NULL;
    //assert(0);
    return vm;
}

int literal_slot(VMState state, Value literal) {
    // (void)state; // suppress compiler warnings
    // (void)literal;
    // Return a slot containing the literal, updating literal pool if needed.
    // For module 1, you can get away with putting the literal in slot 0
    // and returning 0.  For module 2, you'll need something slightly
    // more sophisticated.
    if(state -> curLiteralSize == 0) {
      state -> curLiteralSize += 1;
      state->literals[0] = literal;
      return 0; 
    }
    if(state -> curLiteralSize == 
        (state -> maxLiteralSize - 1)) {
          state -> maxLiteralSize *= 2;
          state -> literals = realloc(state -> literals, 
                                  state->maxLiteralSize * sizeof(struct Value));
    }
    state -> curLiteralSize += 1;
    state -> literals[state -> curLiteralSize - 1] = literal;
    return state -> curLiteralSize - 1;
    // assert(0);
}

// these are for module 2 and beyond

Value literal_value(VMState state, unsigned index) {
  (void) state; (void) index; // replace with real code
  assert(0);
}

int literal_count(VMState state) {
  (void) state; // replace with real code
  assert(0);
}

const char *global_name(VMState state, unsigned index) {
  (void) state; (void) index; // replace with real code
  assert(0);
}
