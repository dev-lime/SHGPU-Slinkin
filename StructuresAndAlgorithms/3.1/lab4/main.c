/*
Разработать модуль squnitA для поддержки стеков и очередей целочисленных значений, реализованных на базе массивов.
Заголовочный файл предоставлятся. Создать программу/набор программ для проверки работоспособности всех функций модуля.
Разработать модуль squnitL для поддержки стеков и очередей целочисленных значений,
реализованных на базе односвязных (для стеков) и двусвязных кольцевых (для очередей) списков.
Интерфейс функций модуля squnitL должен полностью соответствовать интерфейсу функций модулея squnitA.
Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/
/*

// Terminal:
> make
> ./main

*/

#include <stdio.h>
#include <stdlib.h>
#include "squnitA.h"
#include "squnitL.h"

void test_stack_operations(pAStack stack, const char* stack_name) {
    printf("\n=== Тестирование стека (%s) ===\n", stack_name);
    
    // Тест пустого стека
    printf("Пустой стек: %s\n", AStack_empty(stack) ? "ДА" : "НЕТ");
    printf("Количество элементов: %d\n", AStack_count(stack));
    
    // Добавляем элементы
    printf("\nДобавляем элементы 10, 20, 30...\n");
    AStack_push(stack, 10);
    AStack_push(stack, 20);
    AStack_push(stack, 30);
    
    printf("Количество элементов после добавления: %d\n", AStack_count(stack));
    printf("Пустой стек: %s\n", AStack_empty(stack) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!AStack_empty(stack)) {
        int value = AStack_pop(stack);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", AStack_count(stack));
    printf("Пустой стек: %s\n", AStack_empty(stack) ? "ДА" : "НЕТ");
}

void test_queue_operations(paQueue queue, const char* queue_name) {
    printf("\n=== Тестирование очереди (%s) ===\n", queue_name);
    
    // Тест пустой очереди
    printf("Пустая очередь: %s\n", aQueue_empty(queue) ? "ДА" : "НЕТ");
    printf("Количество элементов: %d\n", aQueue_count(queue));
    
    // Добавляем элементы
    printf("\nДобавляем элементы 100, 200, 300...\n");
    aQueue_put(queue, 100);
    aQueue_put(queue, 200);
    aQueue_put(queue, 300);
    
    printf("Количество элементов после добавления: %d\n", aQueue_count(queue));
    printf("Пустая очередь: %s\n", aQueue_empty(queue) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!aQueue_empty(queue)) {
        int value = aQueue_get(queue);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", aQueue_count(queue));
    printf("Пустая очередь: %s\n", aQueue_empty(queue) ? "ДА" : "НЕТ");
}

void test_edge_cases() {
    printf("\n=== Тестирование граничных случаев ===\n");
    
    // Тест стека на массивах с ограниченным размером
    pAStack astack = AStack_create(2);
    printf("\nСтек на массивах (размер=2):\n");
    AStack_push(astack, 1);
    AStack_push(astack, 2);
    printf("Стек полон: %s\n", AStack_full(astack) ? "ДА" : "НЕТ");
    AStack_push(astack, 3); // Должно игнорироваться
    printf("Количество элементов: %d (ожидается 2)\n", AStack_count(astack));
    AStack_destroy(astack);
    
    // Тест очереди на массивах с ограниченным размером
    paQueue aqueue = aQueue_create(3);
    printf("\nОчередь на массивах (размер=3):\n");
    aQueue_put(aqueue, 1);
    aQueue_put(aqueue, 2);
    aQueue_put(aqueue, 3);
    printf("Очередь полна: %s\n", aQueue_full(aqueue) ? "ДА" : "НЕТ");
    aQueue_put(aqueue, 4); // Должно игнорироваться
    printf("Количество элементов: %d (ожидается 3)\n", aQueue_count(aqueue));
    aQueue_destroy(aqueue);
}

int main() {
    printf("=== ТЕСТИРОВАНИЕ МОДУЛЕЙ SQUNIT ===\n");
    
    // Тестирование версии на массивах
    printf("\n*** ВЕРСИЯ НА МАССИВАХ (SQUNITA) ***\n");
    pAStack astack = AStack_create(10);
    paQueue aqueue = aQueue_create(10);
    
    test_stack_operations(astack, "массивы");
    test_queue_operations(aqueue, "массивы");
    
    AStack_destroy(astack);
    aQueue_destroy(aqueue);
    
    // Тестирование версии на списках
    printf("\n*** ВЕРСИЯ НА СПИСКАХ (SQUNITL) ***\n");
    pAStack lstack = AStack_create(10); // параметр игнорируется для списков
    paQueue lqueue = aQueue_create(10); // параметр игнорируется для списков
    
    test_stack_operations(lstack, "списки");
    test_queue_operations(lqueue, "списки");
    
    AStack_destroy(lstack);
    aQueue_destroy(lqueue);
    
    // Тестирование граничных случаев
    test_edge_cases();
    
    printf("\n=== ТЕСТИРОВАНИЕ ЗАВЕРШЕНО ===\n");
    return 0;
}
