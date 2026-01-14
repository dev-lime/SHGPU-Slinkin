#ifndef TREEUNIT_H
#define TREEUNIT_H

typedef struct NTree* PTree;
typedef struct NTree {
	int data;
	PTree left, right;
} NTree;

typedef int (*listfunc)(int);

PTree createNTree(int number);

void pushNTree(PTree* HPTree, PTree ntree);
PTree pullNTree(PTree* HPTree, int number);

void destroyNTree(PTree* HPTree);

void funcNTree(PTree HTRee, int mode, listfunc func);
void printAltNTree(PTree HTRee, int mode);
PTree findNTree(PTree HTRee, int number);

int depthNTree(PTree HTRee, PTree ntree);
int maxDepthNTree(PTree HTRee);
int countNTree(PTree HTRee);
int balancedNTree(PTree HTRee);

#endif
