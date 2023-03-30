#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "linestream.h"
#include "name.h"
#include "print.h"
#include "sxread.h"
#include "value.h"
#include "vmerror.h"
#include "vmheap.h"
#include "vmsizes.h"

typedef Value Sx;
typedef Value Sxlist;

static struct Sourceloc fake_loc = { -1, "standard input" };

//void synerror(Sourceloc src, const char *fmt, ...) __attribute__((noreturn));

_Noreturn
void synerror(Sourceloc src, const char *fmt, ...) {
    va_list_box box;

        assert(fmt);
        fflush(stdout);
        if (!strcmp(src->sourcename, "standard input"))
            fprint(stderr, "syntax error: ");
        else
            fprint(stderr, "syntax error in %s, line %d: ", src->sourcename, src
                                                                        ->line);
        Printbuf buf = printbuf();
        va_start(box.ap, fmt);
        vbprint(buf, fmt, &box);
        va_end(box.ap);

        fwritebuf(buf, stderr);
        freebuf(&buf);
        fprintf(stderr, "\n");
        fflush(stderr);
        abort();
        //longjmp(errorjmp, 1);
}



struct Sxstream {
    Linestream lines;     /* source of more lines */
    const char *input;
                       /* what's not yet read from the most recent input line */
    /* invariant: unread is NULL only if lines is empty */

    struct {
       const char *ps1, *ps2;
    } prompts;
};

Sxstream parstream(Linestream lines, Prompts prompts) {
    Sxstream sxs = malloc(sizeof(*sxs));
    assert(sxs);
    sxs->lines = lines;
    sxs->input = "";
    sxs->prompts.ps1 = prompts == PROMPTING ? "-> " : "";
    sxs->prompts.ps2 = prompts == PROMPTING ? "   " : "";
    return sxs;
}
/*
 * Function [[parsource]] grabs the current source
 * location out of the [[Linestream]].
 * <lex.c>=
 */
Sourceloc parsource(Sxstream sxs) {
    return &sxs->lines->source;
}

static Sx readatom(const char **ps);
static int  isdelim(char c);
static bool brackets_match(char left, char right);

static Sx getsx_in_context(Sxstream sxs, bool is_first, char left) {
    if (sxs->input == NULL)
        return nilValue;
    else {
        char right;      // will hold right bracket, if any
        /*
         * \qbreak To scan past whitespace,
         * [[getsx_in_context]] uses the standard C library
         * function [[isspace]]. That function requires an
         * unsigned character.
         * <advance [[sxs->input]] past whitespace characters>=
         */
        while (isspace((unsigned char)*sxs->input))
            sxs->input++;
        switch (*sxs->input) {
        case '\0':  /* on end of line, get another line and continue */
        case ';':
            sxs->input = getline_(sxs->lines,
                                   is_first ? sxs->prompts.ps1 : sxs->
                                                                   prompts.ps2);
            return getsx_in_context(sxs, is_first, left);
        case '(': case '[': 
            {
                char left = *sxs->input++;
                                         /* remember the opening left bracket */

                Sxlist elems_reversed = emptylistValue;
                Sx q;
                   /* next par read in, to be accumulated into elems_reversed */
                while ((q = getsx_in_context(sxs, false, left)).tag != Nil)
                    elems_reversed = cons(q, elems_reversed);

                if (sxs->input == NULL)
                    synerror(parsource(sxs),

              "premature end of file reading list (missing right parenthesis)");
                else
                    return reverse(elems_reversed);
            }
        case ')': case ']': case '}':
            right = *sxs->input++;
                                 /* pass the bracket so we don't see it again */
            if (is_first) {
                synerror(parsource(sxs), "unexpected right bracket %c", right);
            } else if (left == '\'') {
                synerror(parsource(sxs), "quote ' followed by right bracket %c"
                                                                               ,
                         right);
            } else if (!brackets_match(left, right)) {
                synerror(parsource(sxs), "%c does not match %c", right, left);
            } else {
                return nilValue;
            }
        case '{':
            sxs->input++;
            synerror(parsource(sxs), "curly brackets are not supported");
        default:
            if (*sxs->input == '\'') {
                /*
                 * <read a [[Sx]] and return that [[Sx]] wrapped in [[quote]]
                                                                              >=
                 */
                {
                    sxs->input++;
                    Sx p = getsx_in_context(sxs, false, '\'');
                    if (p.tag == Nil)
                        synerror(parsource(sxs),
                                      "premature end of file after quote mark");
                    return cons(mkStringValue(Vmstring_newc("quote")),
                                cons(p, emptylistValue));
                }
            } else {
                /*
                 * Atoms are delegated to function [[readatom]], defined
                 * below.
                 * <read and return an [[ATOM]]>=
                 */
                return readatom(&sxs->input);
            }
        }   
    }
}

Sx getsx(Sxstream sxs) {
    assert(sxs);
    return getsx_in_context(sxs, true, '\0');
}

static Value readatom(const char **ps) {
    const char *p, *q;

    p = *ps;                          /* remember starting position */
    for (q = p; !isdelim(*q); q++)    /* scan to next delimiter */
        ;
    *ps = q;
                                    /* unconsumed input starts with delimiter */

    VMString contents = Vmstring_new(p, q - p); // adds trailing 0
    char *s = contents->bytes;
    char *t;                        // first nondigit in s
    double x = strtod(s, &t);
                                                 // value of digits in s, if any
    if (*t == '\0' && *s != '\0')   // s is all digits
      return mkNumberValue(x);
    else if (strcmp(s, "#t") == 0)
      return mkBooleanValue(true);
    else if (strcmp(s, "#f") == 0)
      return mkBooleanValue(false);
    else if (strcmp(s, ".") == 0)
      synerror(&fake_loc, "this interpreter cannot handle . in quoted S-expressions");
    else
      return mkStringValue(contents);
}

static int isdelim(char c) {
    return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
                                                                              ||
           c == ';' || isspace((unsigned char)c) || 
           c == '\0';
}
static bool brackets_match(char left, char right) {
    switch (left) {
        case '(': return right == ')';
        case '[': return right == ']';
        case '{': return right == '}';
        default: assert(0);
    }
}




Value cons(Value car, Value cdr) {
  VMNEW(struct VMBlock *, block,  vmsize_cons());
  block->nslots = 2;
  block->slots[0] = car;
  block->slots[1] = cdr;
  return mkConsValue(block);
}

static Sx revapp(Sx xs, Sx ys) {
  if (xs.tag == Emptylist)
    return ys;
  else
    return revapp(cdr(xs), cons(car(xs), ys));
}

Sx reverse(Sx xs) {
  return revapp(xs, emptylistValue);
}

