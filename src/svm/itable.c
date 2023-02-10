//// Look up an opcode by name or by code, get parser and unparser

// Works by simple linear search.  There's nothing here you need.

#define _POSIX_C_SOURCE 200809L



#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "iformat.h"
#include "name.h"
#include "itable.h"
#include "instructions.h"


static inline bool init_called(void) {
  return instructions[0].name != NULL;
}

static struct instruction_info *info_by_code[256];
  // cache for fast indexing by opcode

void itable_init(void) {
  // go from back to front so first entry for each opcode governs
  for (int i = number_of_instructions; i > 0; ) {
    i--;
    instructions[i].name = strtoname(instructions[i].string);
    info_by_code[instructions[i].opcode] = &instructions[i];
  }
}

struct instruction_info *itable_entry(Name n) {
  // to speed this up, could build an index based on first character of name
  assert(init_called());
  for (int i = 0; i < number_of_instructions; i++) {
    if (n == instructions[i].name)
      return instructions + i;
  }
  return NULL;
}

struct instruction_info *itable_entry_by_code(Opcode opcode) {
  assert(init_called());
  assert(opcode == (uint8_t) opcode);
  return info_by_code[opcode];
}

/****************************************************************/

void itable_dump(FILE *fp) {
  int len = 0;
  for (int i = 0; i < number_of_instructions; i++) {
    int n = strlen(instructions[i].string);
    if (n > len)
      len = n;
  }

  for (int i = 0; i < number_of_instructions; i++) {
    fprintf(fp, "%s", instructions[i].string);
    for (int n = strlen(instructions[i].string); n < len; n++)
      fputc(' ', fp);
    fprintf(fp, "  %s%s\n", instructions[i].unparsing_template,
            instructions[i].opcode == Unimp ? " (not yet implemented)" : "");
  }

}
