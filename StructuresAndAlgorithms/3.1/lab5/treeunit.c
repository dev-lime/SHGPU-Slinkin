#include "treeunit.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

PTree createNTree(int number)
{
	PTree r = (PTree)malloc(sizeof(NTree));
	r->data = number; r->left = r->right = NULL;
	return r;
}

void pushNTree(PTree* HPTree, PTree ntree)
{
	if (ntree == NULL) return;
	if (*HPTree == NULL) { *HPTree = ntree; return; }
	PTree r = *HPTree, r1 = NULL;
	while (r != NULL)
	{
		r1 = r;
		if (ntree->data < r->data) r = r->left;
		else r = r->right;
	}
	if (ntree->data < r1->data) r1->left = ntree;
	else r1->right = ntree;
}

PTree pullNTree(PTree* HPTree, int number)
{
	if (*HPTree == NULL) return NULL;
	PTree r = *HPTree, r1 = *HPTree;
	while (r->data != number && r != NULL)
	{
		r1 = r;
		if (number < r->data) r = r->left;
		else r = r->right;
	}
	if (r == NULL) return NULL;
	if (r == *HPTree) *HPTree = r->left;
	else if (number < r1->data) r1->left = r->left;
	else r1->right = r->left;
	pushNTree(HPTree, r->right);
	r->left = r->right = NULL;
	return r;
}

void destroyNTree(PTree* HPTree)
{
	if (*HPTree == NULL) return;
	destroyNTree(&((*HPTree)->left));
	destroyNTree(&((*HPTree)->right));
	free(*HPTree);
	*HPTree = NULL;
}

void funcNTree(PTree HTRee, int mode, listfunc func)
{
	if (HTRee == NULL) return;
	switch (mode)
	{
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

void printAltNTree(PTree HTRee, int mode)
{
	if (HTRee == NULL) return;
	printAltNTree(HTRee->left, mode + 1);
	for (int i = 0; i < mode; i++) printf(">");
	printf("%d\n", HTRee->data);
	printAltNTree(HTRee->right, mode + 1);
}

PTree findNTree(PTree HTRee, int number)
{
	if (HTRee == NULL) return NULL;
	PTree r = HTRee;
	while (r != NULL)
	{
		if (r->data == number) return r;
		if (number < r->data) r = r->left;
		else r = r->right;
	}
	return NULL;
}

int deepthNTree(PTree HTRee, PTree ntree)
{
	if (HTRee == NULL) return -1;
	PTree r = HTRee;
	int depth = 0;
	while (r != NULL)
	{
		if (r == ntree) return depth;
		if (ntree->data < r->data) r = r->left;
		else r = r->right;
		depth++;
	}
	return 0;
}

int maxDeepthNTree(PTree HTRee)
{
	if (HTRee == NULL) return -1;
	PTree r = HTRee;
	int leftd = maxDeepthNTree(r->left);
	int rightd = maxDeepthNTree(r->right);
	return (leftd > rightd ? leftd : rightd) + 1;
}

int countNTree(PTree HTRee)
{
	if (HTRee == NULL) return 0;
	return countNTree(HTRee->left) + countNTree(HTRee->right) + 1;
}

int balancedNTree(PTree HTRee)
{
	if (HTRee == NULL) return 1;
	int leftd = maxDeepthNTree(HTRee->left);
	int rightd = maxDeepthNTree(HTRee->right);
	if (leftd - rightd > 1 || rightd - leftd > 1) return 0;
	if (!balancedNTree(HTRee->left)) return 0;
	if (!balancedNTree(HTRee->right)) return 0;
	return 1;
}
