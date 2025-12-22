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

*/

#include <stdio.h>
#include <stdlib.h>
#include "squnitA.h"
#include "squnitL.h"

void test_array_stack() {
    printf("\n=== Тестирование стека на массивах ===\n");
    
    pAStack stack = AStack_create(10);
    
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
    printf("Полный стек: %s\n", AStack_full(stack) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!AStack_empty(stack)) {
        int value = AStack_pop(stack);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", AStack_count(stack));
    printf("Пустой стек: %s\n", AStack_empty(stack) ? "ДА" : "НЕТ");
    
    AStack_destroy(stack);
}

void test_array_queue() {
    printf("\n=== Тестирование очереди на массивах ===\n");
    
    paQueue queue = aQueue_create(10);
    
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
    printf("Полная очередь: %s\n", aQueue_full(queue) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!aQueue_empty(queue)) {
        int value = aQueue_get(queue);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", aQueue_count(queue));
    printf("Пустая очередь: %s\n", aQueue_empty(queue) ? "ДА" : "НЕТ");
    
    aQueue_destroy(queue);
}

void test_list_stack() {
    printf("\n=== Тестирование стека на списках ===\n");
    
    LStack* stack = LStack_create(10);
    
    // Тест пустого стека
    printf("Пустой стек: %s\n", LStack_empty(stack) ? "ДА" : "НЕТ");
    printf("Количество элементов: %d\n", LStack_count(stack));
    
    // Добавляем элементы
    printf("\nДобавляем элементы 10, 20, 30...\n");
    LStack_push(stack, 10);
    LStack_push(stack, 20);
    LStack_push(stack, 30);
    
    printf("Количество элементов после добавления: %d\n", LStack_count(stack));
    printf("Пустой стек: %s\n", LStack_empty(stack) ? "ДА" : "НЕТ");
    printf("Полный стек: %s\n", LStack_full(stack) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!LStack_empty(stack)) {
        int value = LStack_pop(stack);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", LStack_count(stack));
    printf("Пустой стек: %s\n", LStack_empty(stack) ? "ДА" : "НЕТ");
    
    LStack_destroy(stack);
}

void test_list_queue() {
    printf("\n=== Тестирование очереди на списках ===\n");
    
    LQueue* queue = LQueue_create(10);
    
    // Тест пустой очереди
    printf("Пустая очередь: %s\n", LQueue_empty(queue) ? "ДА" : "НЕТ");
    printf("Количество элементов: %d\n", LQueue_count(queue));
    
    // Добавляем элементы
    printf("\nДобавляем элементы 100, 200, 300...\n");
    LQueue_put(queue, 100);
    LQueue_put(queue, 200);
    LQueue_put(queue, 300);
    
    printf("Количество элементов после добавления: %d\n", LQueue_count(queue));
    printf("Пустая очередь: %s\n", LQueue_empty(queue) ? "ДА" : "НЕТ");
    printf("Полная очередь: %s\n", LQueue_full(queue) ? "ДА" : "НЕТ");
    
    // Извлекаем элементы
    printf("\nИзвлекаем элементы:\n");
    while (!LQueue_empty(queue)) {
        int value = LQueue_get(queue);
        printf("Извлечено: %d\n", value);
    }
    
    printf("Количество элементов после извлечения: %d\n", LQueue_count(queue));
    printf("Пустая очередь: %s\n", LQueue_empty(queue) ? "ДА" : "НЕТ");
    
    LQueue_destroy(queue);
}

void test_edge_cases() {
    printf("\n=== Тестирование граничных случаев ===\n");
    
    // Тест стека на массивах с ограниченным размером
    printf("\n1. Стек на массивах (размер=2):\n");
    pAStack astack = AStack_create(2);
    AStack_push(astack, 1);
    AStack_push(astack, 2);
    printf("Стек полон: %s\n", AStack_full(astack) ? "ДА" : "НЕТ");
    AStack_push(astack, 3); // Должно игнорироваться
    printf("Количество элементов: %d (ожидается 2)\n", AStack_count(astack));
    AStack_destroy(astack);
    
    // Тест очереди на массивах с ограниченным размером
    printf("\n2. Очередь на массивах (размер=3):\n");
    paQueue aqueue = aQueue_create(3);
    aQueue_put(aqueue, 1);
    aQueue_put(aqueue, 2);
    aQueue_put(aqueue, 3);
    printf("Очередь полна: %s\n", aQueue_full(aqueue) ? "ДА" : "НЕТ");
    aQueue_put(aqueue, 4); // Должно игнорироваться
    printf("Количество элементов: %d (ожидается 3)\n", aQueue_count(aqueue));
    aQueue_destroy(aqueue);
    
    // Тест извлечения из пустых структур
    printf("\n3. Извлечение из пустых структур:\n");
    
    pAStack empty_stack = AStack_create(5);
    printf("Извлечение из пустого стека (массив): %d (ожидается -1)\n", AStack_pop(empty_stack));
    AStack_destroy(empty_stack);
    
    paQueue empty_queue = aQueue_create(5);
    printf("Извлечение из пустой очереди (массив): %d (ожидается -1)\n", aQueue_get(empty_queue));
    aQueue_destroy(empty_queue);
    
    LStack* empty_lstack = LStack_create(5);
    printf("Извлечение из пустого стека (список): %d (ожидается -1)\n", LStack_pop(empty_lstack));
    LStack_destroy(empty_lstack);
    
    LQueue* empty_lqueue = LQueue_create(5);
    printf("Извлечение из пустой очереди (список): %d (ожидается -1)\n", LQueue_get(empty_lqueue));
    LQueue_destroy(empty_lqueue);
}

int main() {
    printf("=== ТЕСТИРОВАНИЕ МОДУЛЕЙ SQUNIT ===\n");
    
    // Тестирование версии на массивах
    printf("\n*** ВЕРСИЯ НА МАССИВАХ (SQUNITA) ***\n");
    test_array_stack();
    test_array_queue();
    
    // Тестирование версии на списках
    printf("\n*** ВЕРСИЯ НА СПИСКАХ (SQUNITL) ***\n");
    test_list_stack();
    test_list_queue();
    
    // Тестирование граничных случаев
    test_edge_cases();
    
    printf("\n=== ТЕСТИРОВАНИЕ ЗАВЕРШЕНО ===\n");
    return 0;
}
