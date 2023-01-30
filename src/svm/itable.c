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

void itable_init(void) {
  for (int i = 0; i < number_of_instructions; i++) {
    instructions[i].name = strtoname(instructions[i].string);
  }
  // we could also build a data structure to speed up opcode_info
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
  for (int i = 0; i < number_of_instructions; i++) {
    if (opcode == instructions[i].opcode)
      return instructions + i;
  }
  return NULL;
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
