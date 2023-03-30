#ifndef LINESTREAM_INCLUDED
#define LINESTREAM_INCLUDED

#include <stdio.h>

typedef struct Sourceloc *Sourceloc;

        struct Sourceloc {
            int line;                // current line number
            const char *sourcename;  // where the line came from
        };

typedef struct Linestream *Linestream;

struct Linestream {
    char *buf;               // holds the last line read
    int bufsize;             // size of buf

    struct Sourceloc source; // where the last line came from
    FILE *fin;               // non-NULL if filelines
    const char *s;           // non-NULL if stringlines
};

char *getline_(Linestream r, const char *prompt);


#endif
