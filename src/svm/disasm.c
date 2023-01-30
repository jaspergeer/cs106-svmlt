// Table-driven disassembler and instruction dumper

// You don't need to read this.  The *idea* of a table-driven disassembler 
// is a good one, but the function itself, `printasm`, is hard to read.
// String processing in C is not much fun.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "disasm.h"
#include "iformat.h"
#include "itable.h"
#include "print.h"


//// detailed info about an instruction's operands, for diagnostics
//// (could be exposed in the interface, but isn't)

typedef enum operand {
  // bits used to identify which operands an instruction uses, for diagnostics
  has_uX = 1, has_uY = 2, has_uZ = 4, has_uYZ = 8, has_iXYZ = 16, has_LIT = 32
} Operand;

typedef uint32_t OperandSet;  // one bit per operand
   // in a just world, this would be a struct with 1-bit fields

static OperandSet operands(Instruction i);





void printasm(FILE *fp, VMState vm, Instruction i) {
  Opcode code = opcode(i);
  instruction_info *info = itable_entry_by_code(code);
  if (info == NULL) {
    fprintf(fp, "opcode%d %d %d %d\n", code, uX(i), uY(i), uZ(i));
  } else {
    const char *p = info->unparsing_template;
    assert(p);
    while (*p) {
      size_t n = strcspn(p, "XYZLG");
      if (n > 0 && !strncmp(p + n - 1, "iXYZ", 4)) {
        fwrite(p, 1, n - 1, fp);
        fprintf(fp, "%d", iXYZ(i));
        p += n + 3;
      } else if (!strncmp(p + n, "YZ", 2)) {
        fwrite(p, 1, n, fp);
        fprintf(fp, "%u", uYZ(i));
        p += n + 2;
      } else if (p[n] == '\0') {
        fwrite(p, 1, n, fp);
        break;
      } else {        
        fwrite(p, 1, n, fp);
        switch (p[n]) {
        case 'X': fprintf(fp, "%u", uX(i)); break;
        case 'Y': fprintf(fp, "%u", uY(i)); break;
        case 'Z': fprintf(fp, "%u", uZ(i)); break;
        case 'L':
          if (p[n+1] == 'I' && p[n+2] == 'T') {
            fprint(fp, "%V", literal_value(vm, uYZ(i)));
            p += 2; // we get n + 1 below
          } else {
            fputc('L', fp);
          }
          break;
        case 'G':
          if (strncmp(p+n, "GLOBAL", 6) == 0) {
            fprint(fp, "%s", global_name(vm, uYZ(i)));
            p += 5; // we get n + 1 below
          } else {
            fputc('G', fp);
          }
          break;
        default: assert(0);
        }
        p += n + 1;
      }
    }
//    if (code == LoadLiteral) {
//      fprint(fp, "  ; %V", literal_value(vm, uYZ(i)));
//    }
  }
}

    
static OperandSet operands(Instruction i) {
  OperandSet used = 0;
  Opcode code = opcode(i);
  instruction_info *info = itable_entry_by_code(code);
  if (info != NULL) {
    const char *p = info->unparsing_template;
    if (strstr(p, "XYZ"))
      used |= has_iXYZ;
    else {
      if (strstr(p, "X") && strstr(p, "rX :=") != p)
        used |= has_uX;
      if (strstr(p, "YZ"))
         used |= has_uYZ;
      else if (strstr(p, "LIT"))
         used |= has_LIT;
      else {
        if (strstr(p, "Y"))
          used |= has_uY;
        if (strstr(p, "Z"))
          used |= has_uZ;
      }
    }
  }
  return used;
}      



void idump(FILE *fp, VMState vm, int pc, Instruction I,
           Value *RX, Value *RY, Value *RZ) {
    OperandSet used = operands(I);
    fprintf(fp, "%60s", "");
    if (used & has_uX) { assert(RX); fprint(fp, "  r%d = %V", uX(I), *RX); }
    if (used & has_uY) { assert(RY); fprint(fp, "  r%d = %V", uY(I), *RY); }
    if (used & has_uZ) { assert(RZ); fprint(fp, "  r%d = %V", uZ(I), *RZ); }
    system("tput cr setaf 1");
    fprintf(fp, "[%3d] ", pc);
    system("tput setaf 5");
    printasm(fp, vm, I);
    system("tput op");
    fprintf(fp, "\n");
}


const char *lastglobalset(struct VMState *vm, uint8_t reg, struct VMFunction *f, Instruction *pc) {
  while (--pc >= f->instructions) {
    Instruction i = *pc;
    if (isgetglobal(opcode(i)) && uX(i) == reg) {
      Value gname = literal_value(vm, uYZ(i));
      if (gname.tag == String) {
        return gname.s->bytes;
      } else {
        return NULL;
      }
    }
  }
  return NULL;
}
