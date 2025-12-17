#include "squnitL.h"

// ========== РЕАЛИЗАЦИЯ СТЕКА НА ОДНОСВЯЗНОМ СПИСКЕ ==========

LStack* LStack_create(int maxsize) {
    LStack* stack = (LStack*)malloc(sizeof(LStack));
    if (!stack) return NULL;
    
    stack->top = NULL;
    stack->count = 0;
    return stack;
}

void LStack_destroy(LStack* stack) {
    while (stack->top != NULL) {
        LStackNode* temp = stack->top;
        stack->top = stack->top->next;
        free(temp);
    }
    free(stack);
}

void LStack_push(LStack* stack, int number) {
    LStackNode* newNode = (LStackNode*)malloc(sizeof(LStackNode));
    if (!newNode) return;
    
    newNode->data = number;
    newNode->next = stack->top;
    stack->top = newNode;
    stack->count++;
}

int LStack_pop(LStack* stack) {
    if (stack->top == NULL) return -1;
    
    LStackNode* temp = stack->top;
    int value = temp->data;
    stack->top = stack->top->next;
    free(temp);
    stack->count--;
    return value;
}

int LStack_empty(LStack* stack) {
    return stack->top == NULL;
}

int LStack_full(LStack* stack) {
    // Для списковой реализации стек никогда не бывает полным
    // (ограничено только памятью)
    return 0;
}

int LStack_count(LStack* stack) {
    return stack->count;
}

// ========== РЕАЛИЗАЦИЯ ОЧЕРЕДИ НА ДВУСВЯЗНОМ КОЛЬЦЕВОМ СПИСКЕ ==========

LQueue* LQueue_create(int maxsize) {
    LQueue* queue = (LQueue*)malloc(sizeof(LQueue));
    if (!queue) return NULL;
    
    queue->head = NULL;
    queue->count = 0;
    return queue;
}

void LQueue_destroy(LQueue* que) {
    if (que->head != NULL) {
        LQueueNode* current = que->head;
        do {
            LQueueNode* temp = current;
            current = current->next;
            free(temp);
        } while (current != que->head);
    }
    free(que);
}

void LQueue_put(LQueue* que, int number) {
    LQueueNode* newNode = (LQueueNode*)malloc(sizeof(LQueueNode));
    if (!newNode) return;
    
    newNode->data = number;
    
    if (que->head == NULL) {
        // Первый элемент
        newNode->next = newNode;
        newNode->prev = newNode;
        que->head = newNode;
    } else {
        // Добавляем перед head (в конец очереди)
        LQueueNode* tail = que->head->prev;
        newNode->next = que->head;
        newNode->prev = tail;
        tail->next = newNode;
        que->head->prev = newNode;
    }
    que->count++;
}

int LQueue_get(LQueue* que) {
    if (que->head == NULL) return -1;
    
    LQueueNode* headNode = que->head;
    int value = headNode->data;
    
    if (que->head->next == que->head) {
        // Последний элемент
        free(que->head);
        que->head = NULL;
    } else {
        LQueueNode* newHead = headNode->next;
        LQueueNode* tail = headNode->prev;
        
        tail->next = newHead;
        newHead->prev = tail;
        
        free(headNode);
        que->head = newHead;
    }
    que->count--;
    return value;
}

int LQueue_empty(LQueue* que) {
    return que->head == NULL;
}

int LQueue_full(LQueue* que) {
    // Для списковой реализации очередь никогда не бывает полной
    // (ограничено только памятью)
    return 0;
}

int LQueue_count(LQueue* que) {
    return que->count;
}
