/*
Разработать модуль squnitA для поддержки стеков и очередей целочисленных значений,
реализованных на базе массивов. Заголовочный файл предоставлятся.
Создать программу/набор программ для проверки работоспособности всех функций модуля.

Разработать модуль squnitL для поддержки стеков и очередей целочисленных значений,
реализованных на базе односвязных (для стеков) и двусвязных кольцевых (для очередей) списков.
Интерфейс функций модуля squnitL должен полностью соответствовать интерфейсу функций модулея squnitA.
Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/
/*

// Terminal:
> make clean && make && ./main

error:
Tracer caught signal 11: addr=0x7fc6ad7d2f40 pc=0x7fc6acedaad0 sp=0x7fc6a94ffce0
==5735==LeakSanitizer has encountered a fatal error.
==5735==HINT: For debugging, try setting environment variable LSAN_OPTIONS=verbosity=1:log_threads=1
==5735==HINT: LeakSanitizer does not work under ptrace (strace, gdb, etc)

*/

#include <stdio.h>
#include <stdlib.h>
#include "squnitA.h"
#include "squnitL.h"

int main() {
    printf("=== SQUNITA ===\n");
    
    printf("\n> Стек на массивах:\n");
    pAStack astack = AStack_create(5);
    
    printf("%d\n", AStack_empty(astack));
    //printf("%d\n", AStack_pop(astack));
    
    AStack_push(astack, 10);
    AStack_push(astack, 20);
    AStack_push(astack, 30);
    printf("%d\n", AStack_count(astack));
    
    printf("%d\n", AStack_pop(astack));
    printf("%d\n", AStack_pop(astack));
    printf("%d\n", AStack_count(astack));
    
    AStack_destroy(astack);
    
    printf("\n> Очередь на массивах:\n");
    paQueue aqueue = aQueue_create(5);
    
    printf("%d\n", aQueue_empty(aqueue));
    
    aQueue_put(aqueue, 100);
    aQueue_put(aqueue, 200);
    aQueue_put(aqueue, 300);
    printf("%d\n", aQueue_count(aqueue));
    
    printf("%d\n", aQueue_get(aqueue));
    printf("%d\n", aQueue_get(aqueue));
    printf("%d\n", aQueue_count(aqueue));
    
    aQueue_destroy(aqueue);

    printf("\n=== SQUNITL ===\n");
    
    printf("\n> Стек на списках:\n");
    LStack* lstack = LStack_create(5);
    
    printf("%d\n", LStack_empty(lstack));
    
    LStack_push(lstack, 15);
    LStack_push(lstack, 25);
    LStack_push(lstack, 35);
    printf("%d\n", LStack_count(lstack));
    
    printf("%d\n", LStack_pop(lstack));
    printf("%d\n", LStack_pop(lstack));
    printf("%d\n", LStack_count(lstack));
    
    LStack_destroy(lstack);
    
    printf("\n> Очередь на списках:\n");
    LQueue* lqueue = LQueue_create(5);
    
    printf("%d\n", LQueue_empty(lqueue));
    
    LQueue_put(lqueue, 150);
    LQueue_put(lqueue, 250);
    LQueue_put(lqueue, 350);
    printf("%d\n", LQueue_count(lqueue));
    
    printf("%d\n", LQueue_get(lqueue));
    printf("%d\n", LQueue_get(lqueue));
    printf("%d\n", LQueue_count(lqueue));
    
    LQueue_destroy(lqueue);

    printf("\n=== ГРАНИЧНЫЕ СЛУЧАИ ===\n");
    
    printf("\n> Переполнение стека (массив):\n");
    pAStack small_stack = AStack_create(2);
    AStack_push(small_stack, 1);
    AStack_push(small_stack, 2);
    printf("%d\n", AStack_full(small_stack));
    //AStack_push(small_stack, 3);
    printf("%d\n", AStack_count(small_stack));
    AStack_destroy(small_stack);
    
    printf("\n> Извлечение из пустой очереди (список):\n");
    LQueue* empty_queue = LQueue_create(3);
    printf("%d\n", LQueue_get(empty_queue));
    LQueue_destroy(empty_queue);
    
    printf("\n> Проверка пустоты после очистки:\n");
    LStack* temp_stack = LStack_create(3);
    LStack_push(temp_stack, 99);
    printf("%d\n", LStack_empty(temp_stack));
    LStack_pop(temp_stack);
    printf("%d\n", LStack_empty(temp_stack));
    LStack_destroy(temp_stack);
    return 0;
}
