/*
Разработать модуль для поддержки двусвязных кольцевых списков, заголовочный файл listunit_l2с.h прилагается.
Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/
/*

// Terminal:
> make
> ./main

*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "listunit_l2c.h"

// Тестовые функции
void test_createNode() {
    printf("Testing createNodeL2C...\n");
    pnodeL2C node = createNodeL2C(3.14);
    assert(node != NULL);
    assert(node->data == 3.14);
    assert(node->pprev == node);
    assert(node->pnext == node);
    disposeNodeL2C(&node);
    assert(node == NULL);
    printf("createNodeL2C: OK\n\n");
}

void test_addFirstLast() {
    printf("Testing addFirstNodeL2C and addLastNodeL2C...\n");
    pnodeL2C list = NULL;
    
    // Добавляем первый узел
    pnodeL2C node1 = createNodeL2C(1.0);
    list = node1;
    assert(listCountL2C(list) == 1);
    
    // Добавляем в начало
    pnodeL2C node2 = createNodeL2C(2.0);
    addFirstNodeL2C(&list, node2);
    assert(list->data == 2.0);
    assert(listCountL2C(list) == 2);
    
    // Добавляем в конец
    pnodeL2C node3 = createNodeL2C(3.0);
    addLastNodeL2C(&list, node3);
    assert(listCountL2C(list) == 3);
    
    listOutL2C(list, 1); // Вывод: 2.00 1.00 3.00
    
    disposeListL2C(&list);
    assert(list == NULL);
    printf("addFirstNodeL2C and addLastNodeL2C: OK\n\n");
}

void test_insertOperations() {
    printf("Testing insert operations...\n");
    pnodeL2C list = NULL;
    
    pnodeL2C node1 = createNodeL2C(1.0);
    pnodeL2C node2 = createNodeL2C(2.0);
    pnodeL2C node3 = createNodeL2C(3.0);
    
    list = node1;
    insertAfterNodeL2C(node1, node2); // 1.0 -> 2.0
    insertBeforeNodeL2C(node2, node3); // 1.0 -> 3.0 -> 2.0
    
    assert(listCountL2C(list) == 3);
    assert(list->data == 1.0);
    assert(list->pnext->data == 3.0);
    assert(list->pnext->pnext->data == 2.0);
    
    listOutL2C(list, 1);
    
    disposeListL2C(&list);
    printf("Insert operations: OK\n\n");
}

void test_deleteOperations() {
    printf("Testing delete operations...\n");
    pnodeL2C list = NULL;
    
    pnodeL2C node1 = createNodeL2C(1.0);
    pnodeL2C node2 = createNodeL2C(2.0);
    pnodeL2C node3 = createNodeL2C(3.0);
    
    list = node1;
    addLastNodeL2C(&list, node2);
    addLastNodeL2C(&list, node3);
    
    assert(listCountL2C(list) == 3);
    
    // Удаляем средний узел
    pnodeL2C deleted = deleteNodeL2C(&list, node2);
    assert(deleted == node2);
    assert(listCountL2C(list) == 2);
    
    // Удаляем голову
    deleted = deleteNodeL2C(&list, list);
    assert(deleted == node1);
    assert(list->data == 3.0);
    assert(listCountL2C(list) == 1);
    
    disposeNodeL2C(&deleted);
    
    disposeListL2C(&list);
    printf("Delete operations: OK\n\n");
}

void test_minmax() {
    printf("Testing minmaxL2C...\n");
    pnodeL2C list = NULL;
    
    pnodeL2C node1 = createNodeL2C(5.0);
    pnodeL2C node2 = createNodeL2C(1.0);
    pnodeL2C node3 = createNodeL2C(8.0);
    pnodeL2C node4 = createNodeL2C(3.0);
    
    list = node1;
    addLastNodeL2C(&list, node2);
    addLastNodeL2C(&list, node3);
    addLastNodeL2C(&list, node4);
    
    double min_val = minmaxL2C(list, 1);
    double max_val = minmaxL2C(list, 0);
    
    assert(min_val == 1.0);
    assert(max_val == 8.0);
    
    printf("Min: %.2f, Max: %.2f\n", min_val, max_val);
    
    disposeListL2C(&list);
    printf("minmaxL2C: OK\n\n");
}

void test_listAction() {
    printf("Testing listActionL2C...\n");
    pnodeL2C list = NULL;
    
    pnodeL2C node1 = createNodeL2C(1.0);
    pnodeL2C node2 = createNodeL2C(2.0);
    pnodeL2C node3 = createNodeL2C(3.0);
    
    list = node1;
    addLastNodeL2C(&list, node2);
    addLastNodeL2C(&list, node3);
    
    printf("Forward traversal: ");
    listOutL2C(list, 1);
    
    printf("Backward traversal: ");
    listOutL2C(list, 0);
    
    disposeListL2C(&list);
    printf("listActionL2C: OK\n\n");
}

int main() {
    printf("=== Testing Doubly Linked Circular List Module ===\n\n");
    
    test_createNode();
    test_addFirstLast();
    test_insertOperations();
    test_deleteOperations();
    test_minmax();
    test_listAction();
    
    printf("=== All tests passed! ===\n");
    return 0;
}
