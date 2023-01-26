// -*- c-indent-level: 4; c-basic-offset: 4 -*-

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "vtable.h"
#include "value.h"

void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    STable_free(&vm->globals_by_name);    
    free(vm);
    *sp = NULL;
}

VMState newstate(void) {
    VMState m = calloc(1, sizeof(*m)); // relies on tag for Nil == 0
    assert(m);
    m->globals_by_name = STable_new();
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
    const char *name = AS_CSTRING(vm, nameval);
    const unsigned *slotp = STable_get(vm->globals_by_name, name);
    if (slotp) {
        return (int) *slotp;
    } else {
        int n = vm->nglobals++;
        assert(n < NGLOBALS);
        STable_put(vm->globals_by_name, name, (unsigned) n);
        vm->globals[n].name = strtoname(name);
        return n;
    }
}

const char *global_name(VMState vm, unsigned index) {
    return nametostr(vm->globals[index].name);
}
