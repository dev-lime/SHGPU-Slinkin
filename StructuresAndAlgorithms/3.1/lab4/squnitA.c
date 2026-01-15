#include <stdio.h>
#include <stdlib.h>
#include "squnitA.h"

// ========== РЕАЛИЗАЦИЯ СТЕКА НА МАССИВАХ ==========

pAStack AStack_create(int maxsize) {
    pAStack stack = (pAStack)malloc(sizeof(AStack));
    if (!stack) exit(1);
    
    stack->data = (int*)malloc(sizeof(int) * maxsize);
    if (!stack->data) {
        free(stack);
        exit(1);
    }
    
    stack->maxsize = maxsize;
    stack->count = 0;
    return stack;
}

void AStack_destroy(pAStack stack) {
    if (stack) {
        free(stack->data);
        free(stack);
        return;
    }
    exit(1);
}

void AStack_push(pAStack stack, int number) {
    if (!AStack_full(stack)) {
        stack->data[stack->count] = number;
        stack->count++;
        return;
    }
    exit(1);
}

int AStack_pop(pAStack stack) {
    if (stack->count > 0) {
        stack->count--;
        return stack->data[stack->count];
    }
    exit(1); // ошибка - стек пуст
}

int AStack_empty(pAStack stack) {
    return stack->count == 0;
}

int AStack_full(pAStack stack) {
    return stack && stack->count >= stack->maxsize;
}

int AStack_count(pAStack stack) {
    return stack->count;
}

// ========== РЕАЛИЗАЦИЯ ОЧЕРЕДИ НА МАССИВАХ ==========

paQueue aQueue_create(int maxsize) {
    paQueue que = (paQueue)malloc(sizeof(aQueue));
    if (!que) exit(1);
    
    que->data = (int*)malloc(sizeof(int) * maxsize);
    if (!que->data) {
        free(que);
        exit(1);
    }
    
    que->maxsize = maxsize;
    que->first = 0;
    que->last = 0;
    return que;
}

void aQueue_destroy(paQueue que) {
    if (que) {
        free(que->data);
        free(que);
        return;
    }
    exit(1);
}

void aQueue_put(paQueue que, int number) {
    if (!aQueue_full(que)) {
        que->data[que->last] = number;
        que->last = (que->last + 1) % que->maxsize;
        return;
    }
    exit(1);
}

int aQueue_get(paQueue que) {
    if (!aQueue_empty(que)) {
        int value = que->data[que->first];
        que->first = (que->first + 1) % que->maxsize;
        return value;
    }
    exit(1); // ошибка - очередь пуста
}

int aQueue_empty(paQueue que) {
    return que->first == que->last;
}

int aQueue_full(paQueue que) {
    return (que->last + 1) % que->maxsize == que->first;
}

int aQueue_count(paQueue que) {
    if (que->last >= que->first) {
        return que->last - que->first;
    } else {
        return que->maxsize - que->first + que->last;
    }
}
