// Heart of the VM: runs instructions until told to halt

// You'll write a small `vmrun` function in module 1.  You'll pay
// some attention to performance, but you'll implement only a few 
// instructions.  You'll add other instructions as needed in future modules.

#define _POSIX_C_SOURCE 200809L
#define DENOMINATOR 4294967296

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
  // (void) vm;
  // (void) fun;
  // set up the instruction stream
  vm -> code = fun -> instructions;
  vm -> pc = vm -> code;
  // Run code from `fun` until it executes a Halt instruction.
  // Then return.
  while(1) {
    // get the current instruction
    uint32_t curr_inst = *(vm -> pc);
    switch(opcode(curr_inst)) {
      case Unimp:
        printf("opcode %d not implemented yet\n", opcode(curr_inst));
        break;
      case Halt:
        return;
      case Print:
        print("%v\n", uX(curr_inst));
        break;
      case Check:
        // check(vm, literal index in the YZ field, a register number in the X field)
        check(vm, AS_CSTRING(vm, vm -> literals[uYZ(curr_inst)]), 
                  vm -> registers[uX(curr_inst)]);
        break;
      case Expect:
        expect(vm, AS_CSTRING(vm, vm -> literals[uYZ(curr_inst)]), 
                  vm -> registers[uX(curr_inst)]);
        break;
      case Add:
        (vm -> registers [uX(curr_inst)]).n =
          ((vm -> registers[uY(curr_inst)]).n 
            + (vm -> registers[uZ(curr_inst)]).n) ;
        break;
      case Sub:
        (vm -> registers [uX(curr_inst)]).n =
          ((vm -> registers[uY(curr_inst)]).n 
            - (vm -> registers[uZ(curr_inst)]).n) ;
        break;
      case Multiply:
        (vm -> registers [uX(curr_inst)]).n =
          ((vm -> registers[uY(curr_inst)]).n 
            * (vm -> registers[uZ(curr_inst)]).n) ;
        break;
      case Div:
        (vm -> registers [uX(curr_inst)]).n =
         ((vm -> registers[uY(curr_inst)]).n 
            / (vm -> registers[uZ(curr_inst)]).n);
        break;
      case AND:
        (vm -> registers [uX(curr_inst)]).b =
          (vm -> registers[uY(curr_inst)]).b 
            && (vm -> registers[uZ(curr_inst)]).b;
        break;
      case OR:
        (vm -> registers [uX(curr_inst)]).b =
          (vm -> registers[uY(curr_inst)]).b 
            || (vm -> registers[uZ(curr_inst)]).b;
        break;        
    }
    ++ vm -> pc;
  }
  return;
}
