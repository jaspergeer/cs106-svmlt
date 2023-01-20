// Memory management and literal addition for VMState

// You'll complete this file as part of module 1


#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "value.h"

VMState newstate(void) {
    VMState state = malloc(sizeof(struct VMState));
    state->code = NULL;
    state->pc = NULL;
    return state;
}

void freestatep(VMState *sp) {
    // when free the address of sp, 
    // it overwritten with NULL and will not be reused
    // see Hanson's book
    free(*sp); // what's the difference between free(sp), free(&sp), free(*sp)?
}

int literal_slot(VMState state, Value literal) {
    (void)state; // suppress compiler warnings
    (void)literal;
    // Return a slot containing the literal, updating literal pool if needed.
    // For module 1, you can get away with putting the literal in slot 0
    // and returning 0.  For module 2, you'll need something slightly
    // more sophisticated.
    assert(0);
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
