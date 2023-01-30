// Implementation of the extensible printer.

// This code is gnarly.  If you want to understand it, you're better
// off consulting the Supplement to *Programming Languages: Build,
// Prove, and Compare.*  There you'll find some explanations.

#include <assert.h>
#include <stdbool.h>

#include "name.h"
#include "print.h"
#include "value.h"
#include "vmstring.h"


void bprint(Printbuf output, const char *fmt, ...) {
    va_list_box box;

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(output, fmt, &box);
    va_end(box.ap);
}


void print(const char *fmt, ...) {
    va_list_box box;
    Printbuf stdoutbuf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(stdoutbuf, fmt, &box);
    va_end(box.ap);
    fwritebuf(stdoutbuf, stdout);
    bufreset(stdoutbuf);
    fflush(stdout);
    freebuf(&stdoutbuf);
}

void fprint(FILE *output, const char *fmt, ...) {
    va_list_box box;
    Printbuf buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, output);
    fflush(output);
    freebuf(&buf);
}
static Printer *printertab[256];

void vbprint(Printbuf output, const char *fmt, va_list_box *box) {
    const unsigned char *p;
    bool broken = false;  /* made true on seeing an unknown conversion specifier */
    for (p = (const unsigned char*)fmt; *p; p++) {
        if (*p != '%') {
            bufput(output, *p);
        } else {
            if (!broken && printertab[*++p])
                printertab[*p](output, box);
            else {
                broken = true;  /* box is not consumed */
                bufputs(output, "<pointer>");
            }
        }
    }
}
void installprinter(unsigned char c, Printer *take_and_print) {
    printertab[c] = take_and_print;
}
void printpercent(Printbuf output, va_list_box *box) {
    (void)box;
    bufput(output, '%');
}
void printstring(Printbuf output, va_list_box *box) {
    const char *s = va_arg(box->ap, char*);
    bufputs(output, s);
}

void printdecimal(Printbuf output, va_list_box *box) {
    char buf[2 + 3 * sizeof(int)];
    snprintf(buf, sizeof(buf), "%d", va_arg(box->ap, int));
    bufputs(output, buf);
}

void printhex(Printbuf output, va_list_box *box) {
    char buf[2 + 3 * sizeof(int)];
    snprintf(buf, sizeof(buf), "%x", va_arg(box->ap, int));
    bufputs(output, buf);
}

static void commabuf(Printbuf output, int64_t n) {
  bool leadingzeroes = false;
  if (n < 0) {
    bufputs(output, "-");
    n = (-n);
  }
  if (n > 999) {
    commabuf(output, n / 1000);
    bufputs(output, ",");
    n = n % 1000;
    leadingzeroes = true;
  }
  assert(n >= 0 && n < 1000);
  char buf[4];
  snprintf(buf, sizeof(buf), leadingzeroes ? "%03" PRId64 : "%" PRId64, n);
  bufputs(output, buf);
}

void printcommadecimal(Printbuf output, va_list_box *box) {
    commabuf(output, (long long) va_arg(box->ap, int));
}

void print64commadecimal(Printbuf output, va_list_box *box) {
    commabuf(output, va_arg(box->ap, int64_t));
}

void printpointer(Printbuf output, va_list_box *box) {
    char buf[12 + 3 * sizeof(void *)];
    snprintf(buf, sizeof(buf), "%p", va_arg(box->ap, void *));
    bufputs(output, buf);
}


void printname(Printbuf output, va_list_box *box) {
    Name np = va_arg(box->ap, Name);
    bufputs(output, np == NULL ? "<null>" : nametostr(np));
}

void printchar(Printbuf output, va_list_box *box) {
    int c = va_arg(box->ap, int);
    bufput(output, c);
}


void installprinters(void) {
    installprinter('c', printchar);
    installprinter('d', printdecimal);
    installprinter(',', printcommadecimal);
    installprinter(';', print64commadecimal);
    installprinter('n', printname);
    installprinter('s', printstring);
    installprinter('v', bprintvalue);
    installprinter('V', bprintquotedvalue);
    installprinter('x', printhex);
    installprinter('%', printpercent);
}


static void print_list(Printbuf output, Value v) {
  Value cons = v;
  const char *prefix = "";
  bufputs(output, "(");
  while (cons.tag == ConsCell) {
    bufputs(output, prefix);
    bprint(output, "%v", cons.block->slots[0]);
    prefix = " ";
    cons = cons.block->slots[1];
  }
  if (cons.tag != Emptylist) {
    bufputs(output, " . ");
    bprint(output, "%v", cons);
  }
  bufputs(output, ")");
}
    

void bprintvalue(Printbuf output, va_list_box *box) {
    assert(output);
    assert(box);
    Value v = va_arg(box->ap, Value);
    char buffer[100];
#define OUTPUT(FMT, VALUE) do { \
      snprintf(buffer, sizeof(buffer), FMT, (VALUE)); \
      bufputs(output, buffer); \
    } while (0)

    switch (v.tag) {
    case Nil:     bufputs(output, "nil"); break;
    case Boolean: bufputs(output, v.b ? "#t" : "#f"); break;
    case Number:  {
      double x = v.n;
      long n = (long) x;
      if (x == (double) n)
        OUTPUT("%ld", n);
      else
        OUTPUT("%g", x);
      break;
    }
    case String: 
      bufwrite(output, v.s->bytes, v.s->length);
      break;
    case Table:      OUTPUT("table %p", (void *) v.table); break;
    case Seq:        OUTPUT("sequence %p", (void *) v.seq); break;
    case Block:      OUTPUT("gc-block %p", (void *) v.block); break;
    case CFunction:  OUTPUT("primitive %s", v.cf->name); break;
    case VMFunction: OUTPUT("function %p", (void *) v.f); break;
    case VMClosure:  OUTPUT("closure %p", (void *) v.hof); break;
    case LightUserdata: OUTPUT("userdata %p", (void *) v.p); break;
    case ConsCell:   print_list(output, v); break;
    case Emptylist:  bufputs(output, "'()"); break;
    default: fprintf(stderr, "BAD TAG in print.c <tag=%d=0x%x>\n", v.tag, v.tag); break;
    }
}

void bprintquotedvalue(Printbuf output, va_list_box *box) {
    assert(output);
    assert(box);
    Value v = va_arg(box->ap, Value);
    switch (v.tag) {
    case String: 
      bufput(output, '"');
      bufwrite(output, v.s->bytes, v.s->length);
      bufput(output, '"');
      break;
    default:
      bprint(output, "%v", v);
      break;
    }
}

