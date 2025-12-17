#ifndef SQUNITL
#define SQUNITL

#include <stdio.h>
#include <stdlib.h>

// Структуры для стеков и очередей на списках

// Стек на односвязном списке
typedef struct LStackNode {
    int data;
    struct LStackNode* next;
} LStackNode;

typedef struct LStack {
    LStackNode* top;
    int count;
} LStack;

// Очередь на двусвязном кольцевом списке
typedef struct LQueueNode {
    int data;
    struct LQueueNode* next;
    struct LQueueNode* prev;
} LQueueNode;

typedef struct LQueue {
    LQueueNode* head;
    int count;
} LQueue;

// Функции для работы со стеком (списки)
LStack* LStack_create(int maxsize);
void LStack_destroy(LStack* stack);
void LStack_push(LStack* stack, int number);
int LStack_pop(LStack* stack);
int LStack_empty(LStack* stack);
int LStack_full(LStack* stack);
int LStack_count(LStack* stack);

// Функции для работы с очередью (списки)
LQueue* LQueue_create(int maxsize);
void LQueue_destroy(LQueue* que);
void LQueue_put(LQueue* que, int number);
int LQueue_get(LQueue* que);
int LQueue_empty(LQueue* que);
int LQueue_full(LQueue* que);
int LQueue_count(LQueue* que);

#endif // SQUNITL
