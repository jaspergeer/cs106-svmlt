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
#include "disasm.h"
#include "svmdebug.h"
#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"
#include "string.h"
#include "loader.h"

#define RX registers[uX(curr_inst)]
#define RY registers[uY(curr_inst)]
#define RZ registers[uZ(curr_inst)]

#define LIT LPool_get(literals, uYZ(curr_inst))
#define GLO LPool_get(literals, uYZ(curr_inst))

#define CANDUMP 1

void vmrun(VMState vm, struct VMFunction* fun) {
  vm->pc = 0;
  uint32_t *stream_ptr = fun->instructions + vm->pc;
  Value *registers = vm->registers;
  LPool_T literals = vm->literals;
  Value *globals = vm->globals;

  // for debugging
  const char *dump_decode = svmdebug_value("decode");
  const char *dump_call   = svmdebug_value("call");
  (void) dump_call;
  
  Value *reg0 = &vm->registers[0];
  for (;;) {
    uint32_t curr_inst = *stream_ptr;

    if (CANDUMP && dump_decode) {
      idump(stderr, vm, vm->pc, curr_inst, reg0 - &vm->registers[0],
            reg0+uX(curr_inst), reg0+uY(curr_inst), reg0+uZ(curr_inst));
    }

    switch (opcode(curr_inst)) {
    default:
      print("opcode %d not implemented\n", opcode(curr_inst));
      break;
    case Halt:
      vm->pc = stream_ptr - fun->instructions;
      return;
    case Hash:
      RX = mkNumberValue(hashvalue(RY));
      break;
    case Copy:
      RX = RY;
      break;
    case Err:
      runerror(vm, "%v", RX);
      break;

    // Printing
    case Print:
      print("%v", RX);
      break;
    case Println:
      print("%v\n", RX);
      break;
    case Printu:
      print_utf8(AS_NUMBER(vm, RX));
      break;
    
    // Dynamic Loading
    case PipeOpen: // open pipe, store file descriptor
      {
        FILE *f = popen(AS_CSTRING(vm, LIT), "r");
        RX = mkNumberValue(fileno(f));
      }
      break;
    case DynLoad: // load a list of modules from file
      {
        FILE *input = fdopen(AS_NUMBER(vm, RX), "r");
        // stash registers
        Value reg_stash[NUM_REGISTERS];
        memcpy(reg_stash, registers, NUM_REGISTERS * sizeof(registers[0]));

        for ( struct VMFunction *module = loadmodule(vm, input)
            ; module
            ; module = loadmodule(vm, input)
            ) {
          vmrun(vm, module);
        }
        fclose(input);

        // restore registers
        memcpy(registers, reg_stash, NUM_REGISTERS * sizeof(registers[0]));
      }
      break;

    // Branching
    case CondSkip:
      if (AS_BOOLEAN(vm, RX))
        ++stream_ptr;
      break;
    case Jump:
      *stream_ptr -= iXYZ(curr_inst);
      break;
    
    // Function Calls
    case Call:
      assert(0);
      break;
    case Return:
      assert(0);
      break;
    case TailCall:
      assert(0);
      break;

    // Load/Store
    case LoadLiteral:
      RX = LPool_get(literals, uYZ(curr_inst));
      break;
    case GetGlobal:
      RX = globals[uYZ(curr_inst)];
      break;
    case SetGlobal:
      globals[uYZ(curr_inst)] = RX;
      break;

    // Check-Expect
    case Check:
      check(vm, AS_CSTRING(vm, LIT), RX);
      break;
    case Expect:
      expect(vm, AS_CSTRING(vm, LIT), RX);
      break;
    
    // Arithmetic
    case Zero:
      RX = mkNumberValue(0);
      break;
    case Add:
      RX = mkNumberValue(AS_NUMBER(vm, RY) + AS_NUMBER(vm, RZ));
      break;
    case Sub:
      RX = mkNumberValue(AS_NUMBER(vm, RY) - AS_NUMBER(vm, RZ));
      break;
    case Mul:
      RX = mkNumberValue(AS_NUMBER(vm, RY) * AS_NUMBER(vm, RZ));
      break;
    case Div:
      {
        double uZ_num = AS_NUMBER(vm, RZ);
        if (uZ_num == 0)
          runerror(vm, "divide by zero");
        RX = mkNumberValue(AS_NUMBER(vm, RY) / uZ_num);
      }
      break;
    case Mod:
      RX = mkNumberValue((int) AS_NUMBER(vm, RY) % (int) AS_NUMBER(vm, RZ));
      break;
    case Idiv: 
      {
        int uZ_num = AS_NUMBER(vm, RZ);
        if (uZ_num == 0)
          runerror(vm, "divide by zero");
        RX = mkNumberValue((int) AS_NUMBER(vm, RY) / uZ_num);
      }
      break;
    
    // Boolean Logic
    case Truth:
      RX = mkBooleanValue(GET_TRUTH(vm, RY));
      break;
    case Not:
      RX = mkBooleanValue(!AS_BOOLEAN(vm, RY));
      break;
    case And:
      RX = mkBooleanValue(AS_BOOLEAN(vm, RY) && AS_BOOLEAN(vm, RZ));
      break;
    case Or:
      RX = mkBooleanValue(AS_BOOLEAN(vm, RY) || AS_BOOLEAN(vm, RZ));
      break;
    case Xor:
      RX = mkBooleanValue(AS_BOOLEAN(vm, RY) ^ AS_BOOLEAN(vm, RZ));
      break;

    // Comparison
    case Cmp:
      RX = mkBooleanValue(eqvalue(RY, RZ));
      break;
    case Gt:
      RX = mkBooleanValue(AS_NUMBER(vm, RY) > AS_NUMBER(vm, RZ));
      break;
    case Lt:
      RX = mkBooleanValue(AS_NUMBER(vm, RY) < AS_NUMBER(vm, RZ));
      break;
    case Ge:
      RX = mkBooleanValue(AS_NUMBER(vm, RY) >= AS_NUMBER(vm, RZ));
      break;
    case Le:
      RX = mkBooleanValue(AS_NUMBER(vm, RY) <= AS_NUMBER(vm, RZ));
      break;

    // S-Expressions
    case Cons:
      {
        VMNEW(struct VMBlock *, block,  sizeof *block + 2 * sizeof block->slots[0]);
        block->nslots = 2;
        block->slots[0] = RY;
        block->slots[1] = RZ;
        RX = mkConsValue(block);
      }
      break;
    case Car:
      RX = AS_CONS_CELL(vm, RY)->slots[0];
      break;
    case Cdr:
      RX = AS_CONS_CELL(vm, RY)->slots[1];
      break;

    // type predicates
    case IsFunc:
      RX = mkBooleanValue(isFunction(RY));
      break;
    case IsPair:
      RX = mkBooleanValue(isPair(RY));
      break;
    case IsSym:
      RX = mkBooleanValue(isSymbol(RY));
      break;
    case IsNum:
      RX = mkBooleanValue(isNumber(RY));
      break;
    case IsBool:
      RX = mkBooleanValue(isBoolean(RY));
      break;
    case IsNull:
      RX = mkBooleanValue(isNull(RY));
      break;
    case IsNil:
      RX = mkBooleanValue(isNil(RY));
      break;
    }
    ++stream_ptr;
  }
  return;
}
