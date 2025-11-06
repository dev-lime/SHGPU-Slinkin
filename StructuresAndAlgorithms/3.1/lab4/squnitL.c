#include <stdio.h>
#include <stdlib.h>
#include "squnitL.h"

// ========== РЕАЛИЗАЦИЯ СТЕКА НА ОДНОСВЯЗНОМ СПИСКЕ ==========

pAStack AStack_create(int maxsize) {
    LStack* stack = (LStack*)malloc(sizeof(LStack));
    if (!stack) return NULL;
    
    stack->top = NULL;
    stack->count = 0;
    return (pAStack)stack;
}

void AStack_destroy(pAStack stack) {
    LStack* lstack = (LStack*)stack;
    while (lstack->top != NULL) {
        LStackNode* temp = lstack->top;
        lstack->top = lstack->top->next;
        free(temp);
    }
    free(lstack);
}

void AStack_push(pAStack stack, int number) {
    LStack* lstack = (LStack*)stack;
    LStackNode* newNode = (LStackNode*)malloc(sizeof(LStackNode));
    if (!newNode) return;
    
    newNode->data = number;
    newNode->next = lstack->top;
    lstack->top = newNode;
    lstack->count++;
}

int AStack_pop(pAStack stack) {
    LStack* lstack = (LStack*)stack;
    if (lstack->top == NULL) return -1;
    
    LStackNode* temp = lstack->top;
    int value = temp->data;
    lstack->top = lstack->top->next;
    free(temp);
    lstack->count--;
    return value;
}

int AStack_empty(pAStack stack) {
    LStack* lstack = (LStack*)stack;
    return lstack->top == NULL;
}

int AStack_full(pAStack stack) {
    // Для списковой реализации стек никогда не бывает полным
    // (ограничено только памятью)
    return 0;
}

int AStack_count(pAStack stack) {
    LStack* lstack = (LStack*)stack;
    return lstack->count;
}

// ========== РЕАЛИЗАЦИЯ ОЧЕРЕДИ НА ДВУСВЯЗНОМ КОЛЬЦЕВОМ СПИСКЕ ==========

paQueue aQueue_create(int maxsize) {
    LQueue* queue = (LQueue*)malloc(sizeof(LQueue));
    if (!queue) return NULL;
    
    queue->head = NULL;
    queue->count = 0;
    return (paQueue)queue;
}

void aQueue_destroy(paQueue que) {
    LQueue* lqueue = (LQueue*)que;
    if (lqueue->head != NULL) {
        LQueueNode* current = lqueue->head;
        do {
            LQueueNode* temp = current;
            current = current->next;
            free(temp);
        } while (current != lqueue->head);
    }
    free(lqueue);
}

void aQueue_put(paQueue que, int number) {
    LQueue* lqueue = (LQueue*)que;
    LQueueNode* newNode = (LQueueNode*)malloc(sizeof(LQueueNode));
    if (!newNode) return;
    
    newNode->data = number;
    
    if (lqueue->head == NULL) {
        // Первый элемент
        newNode->next = newNode;
        newNode->prev = newNode;
        lqueue->head = newNode;
    } else {
        // Добавляем перед head (в конец очереди)
        LQueueNode* tail = lqueue->head->prev;
        newNode->next = lqueue->head;
        newNode->prev = tail;
        tail->next = newNode;
        lqueue->head->prev = newNode;
    }
    lqueue->count++;
}

int aQueue_get(paQueue que) {
    LQueue* lqueue = (LQueue*)que;
    if (lqueue->head == NULL) return -1;
    
    LQueueNode* headNode = lqueue->head;
    int value = headNode->data;
    
    if (lqueue->head->next == lqueue->head) {
        // Последний элемент
        free(lqueue->head);
        lqueue->head = NULL;
    } else {
        LQueueNode* newHead = headNode->next;
        LQueueNode* tail = headNode->prev;
        
        tail->next = newHead;
        newHead->prev = tail;
        
        free(headNode);
        lqueue->head = newHead;
    }
    lqueue->count--;
    return value;
}

int aQueue_empty(paQueue que) {
    LQueue* lqueue = (LQueue*)que;
    return lqueue->head == NULL;
}

int aQueue_full(paQueue que) {
    // Для списковой реализации очередь никогда не бывает полной
    // (ограничено только памятью)
    return 0;
}

int aQueue_count(paQueue que) {
    LQueue* lqueue = (LQueue*)que;
    return lqueue->count;
}
