//// Parsing and unparsing for instructions in virtual object code

// This header file specifies the information that the VM knows 
// for each opcode: how it is coded for `vmrun`, what name it
// is known by in the loader (file loader.c), and how it is
// unparsed for disassembly (file disasm.c).

// This file and its implementation itable.c never need to change,
// but when you add new instructions to your SVM, you will need
// to add each instruction to file instructions.c, which uses the 
// format defined here.
//
// (New instructions also need to be added to opcodes.h and to `vmrun`.)

#ifndef ITABLE_INCLUDED
#define ITABLE_INCLUDED

#include <inttypes.h>
#include <stdio.h>

#include "iformat.h"
#include "iparsers.h"

////////////////////////////////////////////////////////////
//
//    Opcode table
//
// Serves two purposes:
//   - Enumerate opcodes with doco, for viz or disassembly
//   - Given an opcode, find a parser

typedef struct instruction_info {
  const char *string;              // name used in virtual object code
  Opcode opcode;                   // name used in `vmrun`
  InstructionParser parser;
  const char *unparsing_template;  // used for disassembly (`disasm`);
                                   // documented in disasm.h
  Name name;                       // == strtoname(string)
} instruction_info;

void itable_init(void);
  // complete the initialization of the instruction table

void itable_dump(FILE *fp);
  // emit human-readable summary of the table

struct instruction_info *itable_entry(Name n);
  // safe to call only after initialization

struct instruction_info *itable_entry_by_code(Opcode opcode);
  // safe to call only after initialization

#endif
