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

static inline Value add(VMState vm, Value a, Value b) {
    return mkNumberValue(AS_NUMBER(vm, a) + AS_NUMBER(vm, b));
}


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
            case Halt:
                return;
            case Print:
                print("%v\n", vm->registers[uX(curr_inst)]);
                break;
            case Check:
                check(vm, AS_CSTRING(vm, vm->literals[uYZ(curr_inst)]), 
                                         vm->registers[uX(curr_inst)]);
                break;
            case Expect:
                expect(vm, AS_CSTRING(vm, vm->literals[uYZ(curr_inst)]),
                                          vm->registers[uX(curr_inst)]);
                break;
            case Add:
                // add the value in uY and uZ and put it in uX
                // project t
                vm->registers[uX(curr_inst)] =
                    add(vm, vm->registers[uY(curr_inst)], 
                            vm->registers[uZ(curr_inst)]);
                break;
            case SetZero:
                // set the value in uX to 0
                vm->registers[uX(curr_inst)] = mkNumberValue(0);
                break;
            case AsBool:
                // examines the Value in uX and make it a boolean Value
                // only cast int to bool, and 0 is false, others are true
                vm->registers[uX(curr_inst)] =
                    mkBooleanValue(AS_BOOLEAN(vm, 
                                              vm->registers[uX(curr_inst)]));
                break;
            case Not:
                // need to implement our own ASBOOLEAN projection function
                /* intersting that copilot generates ASBOOLEAN in this case
                   without ASBOOLEAN being defined */
                // cast the value in uX to boolean, and negate it
                vm->registers[uX(curr_inst)] =
                    mkBooleanValue(!AS_BOOLEAN(vm, vm->registers[uX(curr_inst)]));
                break;
        }
        ++vm->pc;
    }

  return;
}
