// Implementations of the error functions

// Modules < 7: Interesting only if you want to know how to write 
// a function that acts like printf.

// Module 7 onward: useful to extend with a stack trace.

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "value.h"
#include "vmerror.h"

void runerror(VMState state, const char *format, ...) {
  (void)state;
  fprintf(stderr, "Run-time error:\n    ");
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf(stderr, "\n");
  abort();
}

void typeerror(VMState state, const char *expected, Value got, const char *file, int line) {
  (void)state;
  fprintf(stderr, "Run-time error: expected %s, but got %s.\n"
          "\t(Internal source at %s, line %d)\n",
          expected, tagnames[got.tag], file, line);
  abort();
}
