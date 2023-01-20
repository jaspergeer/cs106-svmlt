#ifndef INTQUEUE_H
#define INTQUEUE_H

// typedef struct IQNode* IQNode;

// typedef struct IQNode {
//     int val;
//     struct IQNode *next;
// } *IQNode;

typedef struct IntQueue* IntQueue;

struct IntQueue {
    int *front = nullptr;
    int *back = nullptr;
    int *arr;
    int size;
};

int IQ_enqueue(IntQueue q, int val);
int IQ_dequeue(IntQueue q);
IntQueue IQ_create(int size);
void IQ_free(IntQueue *q);

#endif