#include "treeunit.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

PTree createNTree(int number) {
	PTree r = (PTree)malloc(sizeof(NTree));
	if (r) {
		r->data = number;
		r->left = r->right = NULL;
	}
	return r;
}

void pushNTree(PTree* HPTree, PTree ntree) {
	if (ntree == NULL) return;
	
	// Обработка дубликатов
	if (findNTree(*HPTree, ntree->data) != NULL) {
		free(ntree); // Освобождаем память, т.к. дубликат не нужен
		return;
	}
	
	if (*HPTree == NULL) {
		*HPTree = ntree;
		return;
	}
	
	PTree r = *HPTree, parent = NULL;
	while (r != NULL) {
		parent = r;
		if (ntree->data < r->data) {
			r = r->left;
		} else {
			r = r->right;
		}
	}
	
	if (ntree->data < parent->data) {
		parent->left = ntree;
	} else {
		parent->right = ntree;
	}
}

// Вспомогательная функция для нахождения минимального узла в поддереве
static PTree findMinNode(PTree node) {
	while (node && node->left != NULL) {
		node = node->left;
	}
	return node;
}

PTree pullNTree(PTree* HPTree, int number) {
	if (*HPTree == NULL) return NULL;
	
	PTree parent = NULL, current = *HPTree;
	
	// Поиск узла для удаления
	while (current != NULL && current->data != number) {
		parent = current;
		if (number < current->data) {
			current = current->left;
		} else {
			current = current->right;
		}
	}
	
	if (current == NULL) return NULL; // Узел не найден
	
	// Если узел найден, обрабатываем три случая:
	
	// Случай 1: Узел является листом (нет детей)
	if (current->left == NULL && current->right == NULL) {
		if (parent == NULL) { // Удаляем корень-лист
			*HPTree = NULL;
		} else if (parent->left == current) {
			parent->left = NULL;
		} else {
			parent->right = NULL;
		}
		current->left = current->right = NULL;
		return current;
	}
	
	// Случай 2: Узел имеет только одного ребенка
	if (current->left == NULL || current->right == NULL) {
		PTree child = (current->left != NULL) ? current->left : current->right;
		
		if (parent == NULL) { // Удаляем корень с одним ребенком
			*HPTree = child;
		} else if (parent->left == current) {
			parent->left = child;
		} else {
			parent->right = child;
		}
		
		current->left = current->right = NULL;
		return current;
	}
	
	// Случай 3: Узел имеет двух детей (самый сложный случай)
	// Находим преемника (минимальный узел в правом поддереве)
	PTree successorParent = current;
	PTree successor = current->right;
	
	while (successor->left != NULL) {
		successorParent = successor;
		successor = successor->left;
	}
	
	// Копируем данные преемника в текущий узел
	current->data = successor->data;
	
	// Теперь удаляем преемника (у него не более одного ребенка)
	// Преемник не может иметь левого ребенка, так как он минимальный
	if (successorParent == current) {
		// Преемник - непосредственный правый ребенок
		successorParent->right = successor->right;
	} else {
		// Преемник где-то глубже в левом поддереве
		successorParent->left = successor->right;
	}
	
	successor->left = successor->right = NULL;
	return successor;
}

void destroyNTree(PTree* HPTree) {
	if (*HPTree == NULL) return;
	destroyNTree(&((*HPTree)->left));
	destroyNTree(&((*HPTree)->right));
	free(*HPTree);
	*HPTree = NULL;
}

void funcNTree(PTree HTRee, int mode, listfunc func) {
	if (HTRee == NULL) return;
	switch (mode) {
		case 1: { // Pre-order
			func(HTRee->data);
			funcNTree(HTRee->left, mode, func);
			funcNTree(HTRee->right, mode, func);
			break;
		}
		case 2: { // In-order
			funcNTree(HTRee->left, mode, func);
			func(HTRee->data);
			funcNTree(HTRee->right, mode, func);
			break;
		}
		case 3: { // Post-order
			funcNTree(HTRee->left, mode, func);
			funcNTree(HTRee->right, mode, func);
			func(HTRee->data);
			break;
		}
		case 4: { // rev-pre-order
			func(HTRee->data);
			funcNTree(HTRee->right, mode, func);
			funcNTree(HTRee->left, mode, func);
			break;
		}
		case 5: { // rev-in-order
			funcNTree(HTRee->right, mode, func);
			func(HTRee->data);
			funcNTree(HTRee->left, mode, func);
			break;
		}
		case 6: { // rev-post-order
			funcNTree(HTRee->right, mode, func);
			funcNTree(HTRee->left, mode, func);
			func(HTRee->data);
			break;
		}
		default: break;
	}
}

void printAltNTree(PTree HTRee, int mode) {
	if (HTRee == NULL) return;
	printAltNTree(HTRee->left, mode + 1);
	for (int i = 0; i < mode; i++) printf(">");
	printf("%d\n", HTRee->data);
	printAltNTree(HTRee->right, mode + 1);
}

PTree findNTree(PTree HTRee, int number) {
	if (HTRee == NULL) return NULL;
	PTree r = HTRee;
	while (r != NULL) {
		if (r->data == number) return r;
		if (number < r->data) r = r->left;
		else r = r->right;
	}
	return NULL;
}

int depthNTree(PTree HTRee, PTree ntree) {
	if (HTRee == NULL || ntree == NULL) return -1;
	PTree r = HTRee;
	int depth = 0;
	while (r != NULL) {
		if (r == ntree) return depth;
		if (ntree->data < r->data) r = r->left;
		else r = r->right;
		depth++;
	}
	return -1; // Узел не найден в дереве
}

int maxDepthNTree(PTree HTRee) {
	if (HTRee == NULL) return -1;
	int leftd = maxDepthNTree(HTRee->left);
	int rightd = maxDepthNTree(HTRee->right);
	return (leftd > rightd ? leftd : rightd) + 1;
}

int countNTree(PTree HTRee) {
	if (HTRee == NULL) return 0;
	return countNTree(HTRee->left) + countNTree(HTRee->right) + 1;
}

int balancedNTree(PTree HTRee) {
	if (HTRee == NULL) return 1;
	int leftd = maxDepthNTree(HTRee->left);
	int rightd = maxDepthNTree(HTRee->right);
	if (leftd - rightd > 1 || rightd - leftd > 1) return 0;
	if (!balancedNTree(HTRee->left)) return 0;
	if (!balancedNTree(HTRee->right)) return 0;
	return 1;
}
