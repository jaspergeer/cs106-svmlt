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
    state->free_indices = IQ_create(LITERAL_POOL_SIZE);
    for (int i = 0; i < LITERAL_POOL_SIZE; ++i)
        IQ_enqueue(state->free_indices, i);
    for (int i = 0; i < NUM_REGISTERS; ++i) {
        state->registers[i] = &nilValue;
    }
    return state;
}

void freestatep(VMState *sp) {
    // when free the address of sp, 
    // it overwritten with NULL and will not be reused
    // see Hanson's book
    IQ_free(&(*sp)->free_indices);
    free(*sp);
}

int literal_slot(VMState state, Value literal) {
    int index = IQ_dequeue(state->free_indices);
    state->literals[index] = literal;
    return index;
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
