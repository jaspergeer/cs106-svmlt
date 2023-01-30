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
#include "loader.h"

// static inline Value add(VMState vm, Value a, Value b) {
//     return mkNumberValue(AS_NUMBER(vm, a) + AS_NUMBER(vm, b));
// }

#define RX registers[uX(curr_inst)]
#define RY registers[uY(curr_inst)]
#define RZ registers[uZ(curr_inst)]

#define LIT LPool_get(literals, uYZ(curr_inst))

void vmrun(VMState vm, struct VMFunction* fun) {
  vm->pc = 0;
  uint32_t *stream_ptr = fun->instructions + vm->pc;
  Value *registers = vm->registers;
  LPool_T literals = vm->literals;
  Value *globals = vm->globals;
  (void) globals;

  for (;;) {
    uint32_t curr_inst = *stream_ptr;
    switch (opcode(curr_inst)) {
    default:
      print("opcode %d not implemented\n", opcode(curr_inst));
      break;
    case Halt:
      vm->pc = stream_ptr - fun->instructions;
      return;
    case Print:
      print("%v\n", RX);
      break;
    case CondSkip:
      if (AS_BOOLEAN(vm, RX))
        ++stream_ptr;
      break;
    case Jump:
      stream_ptr += iXYZ(curr_inst);
      break;
    
    // Dynamic Loading
    case PipeOpen: // open pipe, store fd in register
      {
        FILE *f = popen(AS_CSTRING(vm, LIT), "r");
        RX = mkNumberValue(fileno(f));
      }
      break;
    case DynLoad: // load a list of modules from file with descriptor rX
      {
        FILE *input = fdopen(AS_NUMBER(vm, RX), "r");
        for ( struct VMFunction *module = loadmodule(vm, input)
            ; module
            ; module = loadmodule(vm, input)
            ) {
          vmrun(vm, module);
        }
        fclose(input);
      }
      break;

    // Load/Store
    case LoadLiteral:
      RX = LPool_get(literals, uYZ(curr_inst));
      break;
    case GetGLobal:
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
    case Add: // use AS_NUMBER type safe???
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
        int uZ_num = (int) AS_NUMBER(vm, RZ);
        if (uZ_num == 0)
          runerror(vm, "divide by zero");
        RX = mkNumberValue((int) AS_NUMBER(vm, RY) / uZ_num);
      }
      break;
    case Mod:
      RX = mkNumberValue((int) AS_NUMBER(vm, RY) % (int) AS_NUMBER(vm, RZ));
      break;
    
    // Boolean Logic
    case Truth:
      // put the truthiness of the value in uY in uX
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
      RX = mkBooleanValue(AS_NUMBER(vm, RY) >= AS_NUMBER(vm, RZ));
      break;
    }
    ++stream_ptr; // advance the stream pointer
  }
  return;
}
