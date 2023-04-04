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
#include <unistd.h>

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
  Activation *stack_ptr = vm->stack_ptr;
  struct VMFunction *running = fun;

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

    // return is opcode 0
    case Return:
      if (stack_ptr < vm->call_stack)
        return;

      {
        Activation *top = stack_ptr--;
        if (!top->dest_reg)
          runerror(vm, "Tried to return from loading activation");

        *(top->dest_reg) = RX;
        running = top->fun;
        stream_ptr = top->stream_ptr;
        reg0 = top->reg0;
      }
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
      {
      char command[256] = "";
      for (Value curr = RY
          ; !isNull(curr)
          ; curr = AS_CONS_CELL(vm, curr)->slots[1]){
        strcat(command, " ");
        struct VMBlock *currcell = AS_CONS_CELL(vm, curr);
        strcat(command, AS_CSTRING(vm, currcell->slots[0]));
      }

      FILE *input = popen(command, "r");
      RX = mkNumberValue(dup(fileno(input)));
      pclose(input);
      }
      break;
    case DynLoad: // load module at fd stored in RY into RX
      {
        FILE *input = fdopen(AS_NUMBER(vm, RY), "r");
        struct VMFunction *module = loadmodule(vm, input); 
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
        uint8_t lastarg = uZ(instr);
        uint8_t arity = lastarg - funreg;

        struct VMFunction *caller = running;

        const char *funname = lastglobalset(vm, funreg, caller, stream_ptr);
        if (!isFunction(RY)) {
          if (funname)
            runerror(vm, "Tried to call %v; maybe global '%s' is not defined?", RY, funname);
          runerror(vm, "Tried to call %v", RY);
        }
        struct VMFunction *callee = AS_VMFUNCTION(vm, RY);
        
        // arity check
        if (arity != callee->arity)
          runerror(vm, "Called function %s with %d arguments but it requires %d.",
            funname, arity, callee->arity);

        // register overflow check
        if (reg0 + callee->nregs >= vm->registers + (NUM_REGISTERS - 1))
          runerror(vm, "Register file overflow.");

        // stack overflow check
        if (stack_ptr == vm->call_stack + (CALL_STACK_SIZE - 1))
          runerror(vm, "Stack overflow.");

        Activation *top = ++stack_ptr;
        top->stream_ptr = stream_ptr;
        top->reg0 = reg0;
        top->dest_reg = reg0 + destreg;
        top->fun = caller;

        running = caller;
        stream_ptr = callee->instructions - 1;
        reg0 += funreg;
      }
      break;
    case TailCall:
      {      
        uint8_t funreg = uX(instr);
        uint8_t lastarg = uY(instr);
        if (isNil(RX)) {
          runerror(vm, "Tried to call nil; maybe global '%s' is not defined?",
                   lastglobalset(vm, funreg, fun, stream_ptr));
        }

        if (reg0 + lastarg >= vm->registers + (NUM_REGISTERS - 1)) {
          runerror(vm, "Register file overflow");
        }

        // reg file overflow
        if (reg0 + funreg > vm->registers + 255) {
          runerror(vm, "Register file overflow");
        }

        stream_ptr = AS_VMFUNCTION(vm, RX)->instructions - 1;
        memmove(reg0, reg0 + funreg, (lastarg - funreg + 1) * sizeof(Value));        
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
    case CheckAssert:
      check_assert(AS_CSTRING(vm, LIT), RX);
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
