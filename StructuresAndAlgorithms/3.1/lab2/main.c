/*
Разработать модуль listunit_l1.c для поддержки односвязных списков, заголовочный файл listunit_l1.h прилагается: скачать архив шаблона.

В наборе файлов шаблона присутствует файл makefile, предназначенный для упрощения перекомпиляции проекта.
При запуске утилиты make, будет создан исполняемый файл main. При запуске make clean будет удален данный исполняемый файл, а также объектный файл модуля.

Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/
/*

// Terminal:
> make
> ./main

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "listunit_l1.h"

int sum = 0;

int sumNum(char *data)
{
	int num = atoi(data);
	sum += num;
	return 1;
}

int main()
{
    // golova
    // 3 uzla
    // udalit 2' uzel
    // ochistit 2' uzel
    // dobavit uzel v seredinu

	pnodeL1 head = NULL;
	listOutL1(head);

	pnodeL1 node1 = createNodeL1("Первый");
	pnodeL1 node2 = createNodeL1("Второй");
	pnodeL1 node3 = createNodeL1("Третий");
	listOutL1(head);

	addFirstNodeL1(&head, node3);
	listOutL1(head);
	addFirstNodeL1(&head, node2);
	listOutL1(head);
	addFirstNodeL1(&head, node1);
	listOutL1(head);

	pnodeL1 deleteNode = deleteAfterNodeL1(head);
	listOutL1(head);

	disposeNodeL1(&deleteNode);
	listOutL1(head);

	pnodeL1 newNode = createNodeL1("Середина");
	listOutL1(head);
	insertAfterNodeL1(head, newNode);
	listOutL1(head);

	pnodeL1 node4 = createNodeL1("4");
	addLastNodeL1(&head, node4);
	listOutL1(head);

	printf("Count: %d\n", listCountL1(head));

	char buffer[100];
	listSumStr(buffer, sizeof(buffer), head, " | ");
	printf("%s\n", buffer);
	
	pnodeL1 node5 = createNodeL1("5");
	addLastNodeL1(&head, node5);
	listOutL1(head);

	listActionL1(head, sumNum);
	printf("Sum: %d\n", sum);

	disposeListL1(&head);
	listOutL1(head);
	
    return 0;
}
