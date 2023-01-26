// Main program: Dumps the instruction table to standard output

// Useful to call, but not to look at.

#include <stdio.h>

#include "itable.h"


int main (int argc, char *argv[]) {
  (void)argc;
  (void)argv;
  itable_init();
  itable_dump(stdout);
  return 0;
}


