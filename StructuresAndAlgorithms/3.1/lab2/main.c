#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "listunit_l1.h"

int printData(char *data) {
    printf("Data: %s\n", data);
    return 1;
}

int stopAtC(char *data) {
    printf("Checking: %s\n", data);
    if (strcmp(data, "C") == 0) {
        printf("Found 'C', stopping!\n");
        return 0;
    }
    return 1;
}

void testCreateAndDispose() {
    printf("=== Test 1: Create and Dispose ===\n");
    pnodeL1 node = createNodeL1("Test Node");
    if (node != NULL) {
        printf("Created node with data: %s\n", node->data);
        disposeNodeL1(&node);
        printf("Node disposed successfully\n");
    }
    printf("\n");
}

void testAddFirst() {
    printf("=== Test 2: Add First ===\n");
    pnodeL1 list = NULL;
    
    addFirstNodeL1(&list, createNodeL1("First"));
    addFirstNodeL1(&list, createNodeL1("Second"));
    addFirstNodeL1(&list, createNodeL1("Third"));
    
    listOutL1(list);
    printf("List count: %d\n", listCountL1(list));
    
    disposeListL1(&list);
    printf("\n");
}

void testAddLast() {
    printf("=== Test 3: Add Last ===\n");
    pnodeL1 list = NULL;
    
    addLastNodeL1(&list, createNodeL1("A"));
    addLastNodeL1(&list, createNodeL1("B"));
    addLastNodeL1(&list, createNodeL1("C"));
    
    listOutL1(list);
    printf("List count: %d\n", listCountL1(list));
    
    disposeListL1(&list);
    printf("\n");
}

void testInsertAfter() {
    printf("=== Test 4: Insert After ===\n");
    pnodeL1 list = NULL;
    
    pnodeL1 first = createNodeL1("First");
    pnodeL1 second = createNodeL1("Second");
    pnodeL1 middle = createNodeL1("Middle");
    
    addFirstNodeL1(&list, first);
    addLastNodeL1(&list, second);
    insertAfterNodeL1(first, middle);
    
    listOutL1(list);
    
    disposeListL1(&list);
    printf("\n");
}

void testDeleteAfter() {
    printf("=== Test 5: Delete After ===\n");
    pnodeL1 list = NULL;
    
    addLastNodeL1(&list, createNodeL1("One"));
    addLastNodeL1(&list, createNodeL1("Two"));
    addLastNodeL1(&list, createNodeL1("Three"));
    
    printf("Before deletion: ");
    listOutL1(list);
    
    pnodeL1 deleted = deleteAfterNodeL1(list); // Delete after first node
    if (deleted != NULL) {
        printf("Deleted: %s\n", deleted->data);
        disposeNodeL1(&deleted);
    }
    
    printf("After deletion: ");
    listOutL1(list);
    
    disposeListL1(&list);
    printf("\n");
}

void testListAction() {
    printf("=== Test 6: List Action ===\n");
    pnodeL1 list = NULL;
    
    addLastNodeL1(&list, createNodeL1("A"));
    addLastNodeL1(&list, createNodeL1("B"));
    addLastNodeL1(&list, createNodeL1("C"));
    addLastNodeL1(&list, createNodeL1("D"));
    
    printf("Full list traversal:\n");
    listActionL1(list, printData);
    
    printf("\nStopping at 'C':\n");
    listActionL1(list, stopAtC);
    
    disposeListL1(&list);
    printf("\n");
}

void testListSumStr() {
    printf("=== Test 7: List Sum String ===\n");
    pnodeL1 list = NULL;
    
    addLastNodeL1(&list, createNodeL1("Hello"));
    addLastNodeL1(&list, createNodeL1("World"));
    addLastNodeL1(&list, createNodeL1("Test"));
    
    char buffer[100];
    listSumStr(buffer, sizeof(buffer), list, ", ");
    printf("Concatenated: %s\n", buffer);
    
    char smallBuffer[10];
    listSumStr(smallBuffer, sizeof(smallBuffer), list, ", ");
    printf("Truncated: %s\n", smallBuffer);
    
    disposeListL1(&list);
    printf("\n");
}

void testEdgeCases() {
    printf("=== Test 8: Edge Cases ===\n");
    
    // Test with NULL list
    pnodeL1 nullList = NULL;
    printf("Null list count: %d\n", listCountL1(nullList));
    listOutL1(nullList);
    
    // Test disposeAfter on single node
    pnodeL1 singleList = createNodeL1("Single");
    disposeAfterNodeL1(singleList); // Should do nothing
    printf("Single node after disposeAfter: ");
    listOutL1(singleList);
    
    disposeListL1(&singleList);
    printf("\n");
}

int main()
{
    printf("Testing Single Linked List Implementation\n\n");
    
    testCreateAndDispose();
    testAddFirst();
    testAddLast();
    testInsertAfter();
    testDeleteAfter();
    testListAction();
    testListSumStr();
    testEdgeCases();
    
    printf("All tests completed!\n");
    return 0;
}
