/*
Разработать модуль (treeunit.h, treeunit.c) для поддержки бинарных деревьев сортировки.
Структура модуля должна быть аналогична модулям предыдущих ЛР (очереди, стеки, списки).
Модуль должен включать в себя функции:
 создания узла
 вставки узла (нерекурсивно)
 удаления узла (нерекурсивно)
 уничтожения дерева
 операций над деревом с 6 вариантами прохода дерева
 поиска узла (нерекурсивно)
 определения уровня узла (нерекурсивно)
 определения кол-ва узлов дерева
 определения глубины дерева
 определения сбалансированности дерева
Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/

#include <stdio.h>
#include <stdlib.h>
#include "treeunit.h"

int printInt(int i) {
	printf("%d ", i);
	return 1;
}

int main() {
	PTree tree = NULL, tree1 = NULL;
	
	// Тест обработки дубликатов
	printf("Тест обработки дубликатов:\n");
	pushNTree(&tree, createNTree(5));
	pushNTree(&tree, createNTree(3));
	pushNTree(&tree, createNTree(7));
	pushNTree(&tree, createNTree(-1));
	pushNTree(&tree, createNTree(-4));
	pushNTree(&tree, createNTree(2));
	pushNTree(&tree, createNTree(1000));
	pushNTree(&tree, createNTree(4));
	pushNTree(&tree, createNTree(4)); // Дубликат - не добавится
	pushNTree(&tree, createNTree(3)); // Дубликат - не добавится
	
	printf("Все узлы (in-order): ");
	funcNTree(tree, 2, &printInt);
	printf("\n");
	
	// Обходы
	printf("pre-order\t ");
	funcNTree(tree, 1, &printInt);
	printf("\n");
	
	printf("in-order\t ");
	funcNTree(tree, 2, &printInt);
	printf("\n");
	
	printf("post-order\t ");
	funcNTree(tree, 3, &printInt);
	printf("\n");
	
	printf("rev-pre-order\t ");
	funcNTree(tree, 4, &printInt);
	printf("\n");
	
	printf("rev-in-order\t ");
	funcNTree(tree, 5, &printInt);
	printf("\n");
	
	printf("rev-post-order\t ");
	funcNTree(tree, 6, &printInt);
	printf("\n");
	
	printAltNTree(tree, 0);
	
	printf("max depth: %d\n", maxDepthNTree(tree));
	printf("count: %d\n", countNTree(tree));
	printf("balanced: %d\n", balancedNTree(tree));
	
	// Поиск узла
	if ((tree1 = findNTree(tree, 4))) {
		printf("found and depth: %d\n\n", depthNTree(tree, tree1));
	} else {
		printf("not found\n\n");
	}
	
	// Удаление узлов
	printf("Удаление узла 1000:\n");
	PTree removed = pullNTree(&tree, 1000);
	if (removed) {
		free(removed);
		printAltNTree(tree, 0);
		printf("balanced: %d\n\n", balancedNTree(tree));
	}
	
	printf("Удаление узла 3:\n");
	removed = pullNTree(&tree, 3);
	if (removed) {
		free(removed);
		printAltNTree(tree, 0);
		printf("balanced: %d\n\n", balancedNTree(tree));
	}
	
	printf("Удаление корневого узла 5:\n");
	removed = pullNTree(&tree, 5);
	if (removed) {
		free(removed);
		printAltNTree(tree, 0);
		printf("balanced: %d\n\n", balancedNTree(tree));
	}
	
	// Уничтожение дерева
	destroyNTree(&tree);
	printf("destroy\nmax depth: %d\n", maxDepthNTree(tree));
	printf("count: %d\n", countNTree(tree));
	printf("balanced: %d\n", balancedNTree(tree));
	
	return 0;
}
