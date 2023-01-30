#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#define Add Unimp             // blatant cheat for demo
#define ConsInstruction Unimp2 // blatant cheat for demo

#include "vmstate.h"  // for demo, must come first.  Thanks, gcc!
#include "check-expect.h"
#include "disasm.h"
#include "iformat.h"
#include "print.h"
#include "svmdebug.h"
#include "value.h"
#include "vmerror.h"
#include "vmheap.h"
#include "vmrun.h"
#include "vmstate.h"
#include "vmstring.h"
#include "vtable.h"


#define CANDUMP 1

void vmrun(VMState vm, struct VMFunction *fun) {
  vm->running = fun;
  vm->pc = 0;
  Value *reg0 = &vm->registers[0];
  const char* debug = svmdebug_value("decode");

  for (;;) {
    Instruction instr = fun->instructions[vm->pc++];
    if (CANDUMP && debug) {
      idump(stderr, vm, vm->pc, instr,
            reg0+uX(instr), reg0+uY(instr), reg0+uZ(instr));
    }
    switch (opcode(instr)) {
    case Halt:
      return;
    case Check:
      check(vm, AS_CSTRING(vm, vm->literals[uYZ(instr)]), reg0[uX(instr)]);
      break;
    case Expect:
      expect(vm, AS_CSTRING(vm, vm->literals[uYZ(instr)]), reg0[uX(instr)]);
      break;
    case Print:
      print("%v\n", reg0[uX(instr)]);
      break;
    case Add:
      reg0[uX(instr)] =
        mkNumberValue(AS_NUMBER(vm, reg0[uY(instr)]) + AS_NUMBER(vm, reg0[uZ(instr)]));
      break;
    case ConsInstruction: {
        VMNEW(struct VMBlock *, block,  sizeof *block + 2 * sizeof block->slots[0]);
        block->nslots = 2;
        block->slots[0] = reg0[uY(instr)];
        block->slots[1] = reg0[uZ(instr)];
        reg0[uX(instr)] = mkConsValue(block);
        }
        break;
    default:
      runerror(vm, "Unknown opcode %d in instruction 0x%08x\n", opcode(instr), instr);
    }
  }
}
