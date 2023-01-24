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
#include "string.h"

// static inline Value add(VMState vm, Value a, Value b) {
//     return mkNumberValue(AS_NUMBER(vm, a) + AS_NUMBER(vm, b));
// }


void vmrun(VMState vm, struct VMFunction* fun) {
  // instructon stream not used now
  // load state into stack
  // do we want to cache registers?
  // Value registers[NUM_REGISTERS];
  // memcpy(registers, vm->registers, NUM_REGISTERS * sizeof(Value));
  uint32_t* pc = fun->instructions;
  Value* registers = vm->registers;  // reduces one level of indirection
  Value* literals = vm->literals;
  // Value *globals = vm->globals;

  while (1) {
    uint32_t curr_inst = *pc;
    switch (opcode(curr_inst)) {
    default:
      print("opcode %d not implemented\n", opcode(curr_inst));
      break;
    case Halt:
      vm->pc = pc;
      vm->code = fun->instructions;
      return;
    case Print:
      print("%v\n", vm->registers[uX(curr_inst)]);
      break;
    case Check:
      check(vm, AS_CSTRING(vm, literals[uYZ(curr_inst)]),
        registers[uX(curr_inst)]);
      break;
    case Expect:
      expect(vm, AS_CSTRING(vm, literals[uYZ(curr_inst)]),
        registers[uX(curr_inst)]);
      break;
    case SetZero:
      // set the value in uX to 0
      registers[uX(curr_inst)] = mkNumberValue(0);
      break;
    case GetTruth:
      // put the truthiness of the value in uY in uX
      registers[uX(curr_inst)] = mkBooleanValue(GET_TRUTH(vm, registers[uY(curr_inst)]));
      break;
    case Not:
      registers[uX(curr_inst)] =
        mkBooleanValue(!AS_BOOLEAN(vm, registers[uY(curr_inst)]));
      break;
    case Add: // use AS_NUMBER type safe???
      registers[uX(curr_inst)] = mkNumberValue(AS_NUMBER(vm ,registers[uY(curr_inst)]) +
        AS_NUMBER(vm, registers[uZ(curr_inst)]));
      break;
    case Sub:
      registers[uX(curr_inst)] = mkNumberValue(AS_NUMBER(vm ,registers[uY(curr_inst)]) -
        AS_NUMBER(vm, registers[uZ(curr_inst)]));
      break;
    case Mul:
      registers[uX(curr_inst)] = mkNumberValue(AS_NUMBER(vm ,registers[uY(curr_inst)]) *
        AS_NUMBER(vm, registers[uZ(curr_inst)]));
      break;
    case Div:
      {
      int uZ_num = (int) AS_NUMBER(vm, registers[uZ(curr_inst)]);
      if (uZ_num == 0)
        runerror(vm, "divide by zero");
      registers[uX(curr_inst)] = mkNumberValue((int) AS_NUMBER(vm , registers[uY(curr_inst)]) /
        uZ_num);
      }
      break;
    case And:
      registers[uX(curr_inst)] = mkBooleanValue(AS_BOOLEAN(vm ,registers[uY(curr_inst)]) &&
        AS_BOOLEAN(vm, registers[uZ(curr_inst)]));
      break;
    case Or:
      registers[uX(curr_inst)] = mkBooleanValue(AS_BOOLEAN(vm ,registers[uY(curr_inst)]) ||
        AS_BOOLEAN(vm, registers[uZ(curr_inst)]));
      break;
    }
    ++pc; // advance the program counter
  }
  return;
}
