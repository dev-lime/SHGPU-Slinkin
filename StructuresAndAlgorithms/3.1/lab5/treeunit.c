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

void funcNTree(PTree HTree, int mode, listfunc func) {
	if (HTree == NULL) return;
	switch (mode) {
		case 1: { // Pre-order
			func(HTree->data);
			funcNTree(HTree->left, mode, func);
			funcNTree(HTree->right, mode, func);
			break;
		}
		case 2: { // In-order
			funcNTree(HTree->left, mode, func);
			func(HTree->data);
			funcNTree(HTree->right, mode, func);
			break;
		}
		case 3: { // Post-order
			funcNTree(HTree->left, mode, func);
			funcNTree(HTree->right, mode, func);
			func(HTree->data);
			break;
		}
		case 4: { // rev-pre-order
			func(HTree->data);
			funcNTree(HTree->right, mode, func);
			funcNTree(HTree->left, mode, func);
			break;
		}
		case 5: { // rev-in-order
			funcNTree(HTree->right, mode, func);
			func(HTree->data);
			funcNTree(HTree->left, mode, func);
			break;
		}
		case 6: { // rev-post-order
			funcNTree(HTree->right, mode, func);
			funcNTree(HTree->left, mode, func);
			func(HTree->data);
			break;
		}
		default: break;
	}
}

void printAltNTree(PTree HTree, int mode) {
	if (HTree == NULL) return;
	printAltNTree(HTree->left, mode + 1);
	for (int i = 0; i < mode; i++) printf(">");
	printf("%d\n", HTree->data);
	printAltNTree(HTree->right, mode + 1);
}

PTree findNTree(PTree HTree, int number) {
	if (HTree == NULL) return NULL;
	PTree r = HTree;
	while (r != NULL) {
		if (r->data == number) return r;
		if (number < r->data) r = r->left;
		else r = r->right;
	}
	return NULL;
}

int depthNTree(PTree HTree, PTree ntree) {
	if (HTree == NULL || ntree == NULL) return -1;
	PTree r = HTree;
	int depth = 0;
	while (r != NULL) {
		if (r == ntree) return depth;
		if (ntree->data < r->data) r = r->left;
		else r = r->right;
		depth++;
	}
	return -1; // Узел не найден в дереве
}

int maxDepthNTree(PTree HTree) {
	if (HTree == NULL) return -1;
	int leftd = maxDepthNTree(HTree->left);
	int rightd = maxDepthNTree(HTree->right);
	return (leftd > rightd ? leftd : rightd) + 1;
}

int countNTree(PTree HTree) {
	if (HTree == NULL) return 0;
	return countNTree(HTree->left) + countNTree(HTree->right) + 1;
}

int balancedNTree(PTree HTree) {
	if (HTree == NULL) return 1;
	int leftd = maxDepthNTree(HTree->left);
	int rightd = maxDepthNTree(HTree->right);
	if (leftd - rightd > 1 || rightd - leftd > 1) return 0;
	if (!balancedNTree(HTree->left)) return 0;
	if (!balancedNTree(HTree->right)) return 0;
	return 1;
}
