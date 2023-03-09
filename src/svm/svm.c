// Main program: Loads and runs SVM modules

// Defines a classic `main` function with a classic structure:
//
//   1. Initialize modules that need initialization.
//   2. Process either `stdin` or every file named on the command line.
//   3. Finalize modules that need finalization.
//     
// If any argument is "--", then the rest of the arguments are placed
// into global variable `argv` as a list.  This placement occurs before
// any code is run.  Eventually each argument will be parsed as an
// S-expression, but for now it's just a string.
//
// If I've done my job, you don't need to edit this.  Or even look at it.

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "argparse.h"
#include "check-expect.h"
#include "loader.h"
#include "itable.h"
#include "print.h"
#include "svmdebug.h"
#include "vmheap.h"
#include "vmrun.h"
#include "vmstate.h"
#include "vmstring.h"

static void dofile(struct VMState *vm, FILE *input) { 
  for ( struct VMFunction *module = loadmodule(vm, input)
      ; module
      ; module = loadmodule(vm, input)
      ) {
    vmrun(vm, module);
  }
  report_unit_tests();
}

int main(int argc, char **argv) {
    itable_init();
    Vmstring_init();
    installprinters();
    heap_init();
    VMState vm = newstate();

    int input_file_limit; // one beyond last input file
    for (input_file_limit = 1; input_file_limit < argc; input_file_limit++)
      if (!strcmp(argv[input_file_limit], "--"))
        break;
    
    initialize_global(vm, mkStringValue(Vmstring_newc("argv")),
                      arglist(argc - input_file_limit - 1, argv+input_file_limit + 1));

    if (input_file_limit == 1) {
      dofile(vm, stdin);
    } else {
      for (int i = 1; i < input_file_limit; i++) {
        FILE *exe = strcmp(argv[i], "-") == 0 ? stdin : fopen(argv[i], "r");
        assert(exe);
        dofile(vm, exe);
        if (exe != stdin)
          fclose(exe);
      }
    }
    freestatep(&vm);
    heap_shutdown();
    name_cleanup();
    Vmstring_finish();
    svmdebug_finalize();
    return EXIT_SUCCESS;
}
