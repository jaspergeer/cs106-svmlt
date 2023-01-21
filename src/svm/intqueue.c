#include "intqueue.h"
#include <stdlib.h>

void IQ_enqueue(IntQueue q, int val) {
    ++q->back;
    if (q->back >= q->arr + q->size)
        q->back = q->arr;
    
    // resize ?

    *q->back = val;
}

int IQ_dequeue(IntQueue q) {
    int val = *q->front;

    ++q->front;
    if (q->front >= q->arr + q->size)
        q->front = q->arr;
    
    return val;
}

IntQueue IQ_create(int size) {
    IntQueue q = malloc(sizeof(struct IntQueue));
    q->arr = malloc(size);
    q->front = q->arr;
    q->back = q->arr;
    return q;
}

void IQ_free(IntQueue *q) {
    free(*q);
}
