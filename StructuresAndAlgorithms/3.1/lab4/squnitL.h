#ifndef SQUNITL
#define SQUNITL

#include "squnitA.h" // для совместимости интерфейсов

// Переопределяем структуры для совместимости
typedef struct AStack *pAStack;
typedef struct aQueue *paQueue;

// Создаем новые структуры для списков
typedef struct LStackNode {
    int data;
    struct LStackNode* next;
} LStackNode;

typedef struct LStack {
    LStackNode* top;
    int count;
} LStack;

typedef struct LQueueNode {
    int data;
    struct LQueueNode* next;
    struct LQueueNode* prev;
} LQueueNode;

typedef struct LQueue {
    LQueueNode* head;
    int count;
} LQueue;

// Функции для работы со стеком (односвязный список)
pAStack AStack_create(int maxsize);
void AStack_destroy(pAStack stack);
void AStack_push(pAStack stack, int number);
int AStack_pop(pAStack stack);
int AStack_empty(pAStack stack);
int AStack_full(pAStack stack);
int AStack_count(pAStack stack);

// Функции для работы с очередью (двусвязный кольцевой список)
paQueue aQueue_create(int maxsize);
void aQueue_destroy(paQueue que);
void aQueue_put(paQueue que, int number);
int aQueue_get(paQueue que);
int aQueue_empty(paQueue que);
int aQueue_full(paQueue que);
int aQueue_count(paQueue que);

#endif // SQUNITL
