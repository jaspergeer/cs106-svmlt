// Extensible printer, as in PL: Build, Prove, Compare

// Useful for debugging: call print() or fprint() with %v, %n, others.
// Worth knowing about from module 1 onward.


#ifndef PRINT_INCLUDED
#define PRINT_INCLUDED

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct va_list_box {
  va_list ap;
} va_list_box;

typedef struct Printbuf *Printbuf;
typedef void Printer(Printbuf output, va_list_box *args);



//// configuration for the printer, including initialization

void installprinter(unsigned char specifier, Printer *take_and_print);
void installprinters(void);
  // installs formats %c, %d, %s, %%, and
  //   %n (Name)
  //   %v (Value)

Printer printpercent, printstring, printdecimal, printchar, printname, printpointer;
Printer printpar, bprintvalue, bprintquotedvalue;

//// print to files

void print (const char *fmt, ...);                /* print to standard output */
void fprint(FILE *output, const char *fmt, ...);     /* print to given file */
void vfprint(FILE *output, const char *fmt, va_list_box *box);  /* print to given file */
  // N.B. these functions allocate and deallocate a Printbuf at each call.  

//// print to a buffer

void bprint(Printbuf output, const char *fmt, ...);  /* print to given buffer */
void vbprint(Printbuf output, const char *fmt, va_list_box *box);


//// UTF-8 printers (probably ought to be converted to a %U escape)

void fprint_utf8(FILE *output, unsigned code_point);
void print_utf8 (unsigned u);



//// low-level buffer functions

Printbuf printbuf(void);
void freebuf(Printbuf *);
void bufput(Printbuf, char);
void bufputs(Printbuf, const char*);
void bufwrite(Printbuf, const char*, size_t);
void bufreset(Printbuf);
char *bufcopy(Printbuf);
void fwritebuf(Printbuf buf, FILE *output);



#endif
