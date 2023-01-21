// Heart of the VM: runs instructions until told to halt

// You'll write a small `vmrun` function in module 1.  You'll pay
// some attention to performance, but you'll implement only a few 
// instructions.  You'll add other instructions as needed in future modules.

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "check-expect.h"
#include "iformat.h"
#include "value.h"
#include "vmstate.h"
#include "vmrun.h"

#include "print.h"

#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"


void vmrun(VMState vm, struct VMFunction *fun) {
    (void) vm;
    (void) fun;
    vm->code = fun->instructions;
    vm->pc = vm->code;

    while (1) {
        uint32_t curr_inst = *(vm->pc);
        switch(opcode(*(vm->pc))) {
            default:
                print("opcode %d not implemented\n", opcode(curr_inst));
                break;
            case Print:
                print("%v\n", *(vm->registers[uX(curr_inst)]));
                break;
            case Check:
                check(vm, AS_CSTRING(vm, vm->literals[uYZ(curr_inst)]), 
                          *(vm->registers[uX(curr_inst)]));
                break;
            case Expect:
                expect(vm, AS_CSTRING(vm, vm->literals[uYZ(curr_inst)]), *(vm->registers[uX(curr_inst)]));
                break;
            case Halt:
                return; // and more stuff
        }
        ++vm->pc;
    }

  return;
}
