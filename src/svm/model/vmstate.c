// -*- c-indent-level: 4; c-basic-offset: 4 -*-

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "vtable.h"
#include "value.h"
#include "name.h"

void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    free(vm);
    *sp = NULL;
}

VMState newstate(void) {
    VMState m = calloc(1, sizeof(*m)); // relies on tag for Nil == 0
    assert(m);
    return m;
}

int literal_slot(VMState state, Value literal) {
    int n = state->nlits++;
    assert(n < LITSIZE);
    state->literals[n] = literal;
    return n;
}

Value literal_value(VMState vm, unsigned index) {
    return vm->literals[index];
}

int literal_count(VMState vm) {
    return vm->nlits;
}

int global_slot(VMState vm, Value nameval) {
    Name name = strtoname(AS_CSTRING(vm, nameval));
    int slot;
    for (slot = 0; slot < vm->nglobals; slot++) {
      if (vm->global_names[slot] == name)
        return slot;
    }
    slot = vm->nglobals++;
    assert(slot < NGLOBALS);
    vm->global_names[slot] = name;
    return slot;
}

const char *global_name(VMState vm, unsigned index) {
    return nametostr(vm->global_names[index]);
}
