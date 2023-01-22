// Main program; tests a few SVM instructions

// Module 1: The easy way to test your code.
// Module 2 onward: This file will be obsolete; use `svm.c` instead.


#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "check-expect.h"
#include "testfuns.h"
#include "print.h"
#include "vmheap.h"
#include "vmrun.h"
#include "vmstate.h"
#include "vmstring.h"

int main(int argc, char **argv) {
    assert(argc == 1);
    (void)argv;
    Vmstring_init();
    installprinters();
    heap_init();
    VMState vm = newstate();

    fprintf(stderr, "Test: halt\n");
    vmrun(vm, haltfun());
    fprintf(stderr, "Test: print $r0; halt\n");
    vmrun(vm, print0fun());
    fprintf(stderr, "Test: check $r0, ...; expect $r0, ...; halt\n");
    vmrun(vm, ce0fun(vm));
    //fprintf(stderr, "Test: add $r1 $r2 expect $r0 = $r1 + $r2; halt\n");
    report_unit_tests();

    freestatep(&vm);
    heap_shutdown();
    Vmstring_finish();
    return EXIT_SUCCESS;
}
