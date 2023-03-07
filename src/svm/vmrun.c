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

#define RX reg0[uX(instr)]
#define RY reg0[uY(instr)]
#define RZ reg0[uZ(instr)]

#define LIT LPool_get(literals, uYZ(instr))
#define GLO LPool_get(literals, uYZ(instr))

#define CANDUMP 1

void vmrun(VMState vm, struct VMFunction* fun) {
  Instruction *stream_ptr = fun->instructions;
  LPool_T literals = vm->literals;
  Value *globals = vm->globals;

  // for debugging
  const char *dump_decode = svmdebug_value("decode");
  const char *dump_call   = svmdebug_value("call");
  (void) dump_call;
  
  Value *reg0 = vm->registers;
  for (;;) {
    uint32_t instr = *stream_ptr;

    if (CANDUMP && dump_decode) {
      idump(stderr, vm, vm->pc, instr, reg0 - &vm->registers[0],
            reg0+uX(instr), reg0+uY(instr), reg0+uZ(instr));
    }

    switch (opcode(instr)) {
    default:
      print("opcode %d not implemented\n", opcode(instr));
      break;
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
      RX = mkNumberValue(fileno(popen(AS_CSTRING(vm, LIT), "r")));
      break;
    case DynLoad: // load module at fd stored in RY into RX
      {
        FILE *input = fdopen(AS_NUMBER(vm, RY), "r");
        struct VMFunction *module = loadmodule(vm, fdopen(AS_NUMBER(vm, RY), "r")); 
        if (!module)
          runerror(vm, "Missing or malformed module");
        RX = mkVMFunctionValue(module);
        fclose(input);
      }
      break;

    // Branching
    case CondSkip:
      if (!AS_BOOLEAN(vm, RX))
        ++stream_ptr;
      break;
    case Jump:
      stream_ptr += iXYZ(instr);
      break;
    
    // Function Calls
    case Call:
      {
        uint8_t destreg = uX(instr);
        uint8_t funreg = uY(instr);

        if (isNil(RY)) {
          runerror(vm, "Tried to call nil; maybe global '%s' is not defined?",
                   lastglobalset(vm, funreg, fun, stream_ptr));
        }

        // stack is full
        if (vm->stack_ptr == vm->call_stack + (CALL_STACK_SIZE - 1)) {
          runerror(vm, "Stack overflow");
        }
        // fprintf(stderr, "stack is called\n");

        Activation *top = ++vm->stack_ptr;
        top->stream_ptr = stream_ptr;
        top->reg0 = reg0;
        top->dest_reg = reg0 + destreg;

        stream_ptr = AS_VMFUNCTION(vm, RY)->instructions - 1;
        reg0 += funreg;
      }
      break;
    case Return:
      if (vm->stack_ptr < vm->call_stack)
        return;

      {
        Activation *top = vm->stack_ptr--;
        if (!top->dest_reg)
          runerror(vm, "Tried to return from loading activation");

        *(top->dest_reg) = RX;
        stream_ptr = top->stream_ptr;
        reg0 = top->reg0;
      }
      break;
    case TailCall:
      {
        // printf("tail call\n");
      
        uint8_t funreg = uX(instr);
        uint8_t lastarg = uY(instr);

        // reg file overflow
        if (reg0 + funreg > vm->registers + 255) {
          runerror(vm, "Register file overflow");
        }

        memmove(reg0, reg0 + funreg, (lastarg - funreg + 1) * sizeof(Value));
        stream_ptr = AS_VMFUNCTION(vm, RX)->instructions - 1;
      }
      break;

    // Load/Store
    case LoadLiteral:
      RX = LPool_get(literals, uYZ(instr));
      break;
    case GetGlobal:
      RX = globals[uYZ(instr)];
      break;
    case SetGlobal:
      globals[uYZ(instr)] = RX;
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
