#include <stdio.h>
#include <stdlib.h>
#include "listunit_l2c.h"

// создает узел и возвращает его
pnodeL2C createNodeL2C(double data) {
    pnodeL2C newNode = (pnodeL2C)malloc(sizeof(tnodeL2C));
    if (newNode == NULL) {
        return NULL;
    }
    newNode->data = data;
    newNode->pprev = newNode;
    newNode->pnext = newNode;
    return newNode;
}

// вставляет узел в начало списка и возвращает вставленный узел
pnodeL2C addFirstNodeL2C(pnodeL2C *ph, pnodeL2C p) {
    if (p == NULL) return NULL;
    
    if (*ph == NULL) {
        *ph = p;
        return p;
    }
    
    // Вставляем перед головой
    p->pnext = *ph;
    p->pprev = (*ph)->pprev;
    (*ph)->pprev->pnext = p;
    (*ph)->pprev = p;
    *ph = p;
    
    return p;
}

// вставляет узел в конец списка и возвращает вставленный узел
pnodeL2C addLastNodeL2C(pnodeL2C *ph, pnodeL2C p) {
    if (p == NULL) return NULL;
    
    if (*ph == NULL) {
        *ph = p;
        return p;
    }
    
    // Вставляем перед головой (это конец в кольцевом списке)
    p->pnext = *ph;
    p->pprev = (*ph)->pprev;
    (*ph)->pprev->pnext = p;
    (*ph)->pprev = p;
    
    return p;
}

// вставляет узел p в список после узла pn и возвращает вставленный узел
pnodeL2C insertAfterNodeL2C(pnodeL2C pn, pnodeL2C p) {
    if (pn == NULL || p == NULL) return NULL;
    
    p->pnext = pn->pnext;
    p->pprev = pn;
    pn->pnext->pprev = p;
    pn->pnext = p;
    
    return p;
}

// вставляет узел p в список перед узлом pn и возвращает вставленный узел
pnodeL2C insertBeforeNodeL2C(pnodeL2C pn, pnodeL2C p) {
    if (pn == NULL || p == NULL) return NULL;
    
    p->pnext = pn;
    p->pprev = pn->pprev;
    pn->pprev->pnext = p;
    pn->pprev = p;
    
    return p;
}

// удаляет узел из списка и возвращает удаленный узел
pnodeL2C deleteNodeL2C(pnodeL2C *ph, pnodeL2C pn) {
    if (pn == NULL) return NULL;
    
    // Если список содержит только один узел
    if (pn->pnext == pn) {
        *ph = NULL;
    } else {
        pn->pprev->pnext = pn->pnext;
        pn->pnext->pprev = pn->pprev;
        
        // Если удаляем голову, обновляем указатель на голову
        if (pn == *ph) {
            *ph = pn->pnext;
        }
    }
    
    // Изолируем удаленный узел
    pn->pnext = pn;
    pn->pprev = pn;
    
    return pn;
}

// уничтожает узел и устанавливает его в NULL
void disposeNodeL2C(pnodeL2C *pn) {
    if (pn == NULL || *pn == NULL) return;
    
    free(*pn);
    *pn = NULL;
}

// уничтожает список и устанавливает его в NULL
void disposeListL2C(pnodeL2C *ph) {
    if (ph == NULL || *ph == NULL) return;
    
    pnodeL2C current = *ph;
    pnodeL2C next;
    
    do {
        next = current->pnext;
        free(current);
        current = next;
    } while (current != *ph && current != NULL);
    
    *ph = NULL;
}

// выполняет функцию func над каждым узлом списка
void listActionL2C(pnodeL2C ph, int fwd, listfunc func) {
    if (ph == NULL || func == NULL) return;
    
    pnodeL2C current = ph;
    
    do {
        if (!func(current->data)) {
            break;
        }
        current = fwd ? current->pnext : current->pprev;
    } while (current != ph);
}

// Вспомогательная функция для вывода
static int printNode(double data) {
    printf("%.2f ", data);
    return 1;
}

// выводит список в стандартный поток вывода
void listOutL2C(pnodeL2C ph, int fwd) {
    if (ph == NULL) {
        printf("Empty list\n");
        return;
    }
    
    printf("List (%s): ", fwd ? "forward" : "backward");
    listActionL2C(ph, fwd, printNode);
    printf("\n");
}

// Вспомогательная функция для подсчета
static int countNodes(double data) {
    (void)data; // Неиспользуемый параметр
    return 1;
}

// возвращает количество элементов в списке
int listCountL2C(pnodeL2C ph) {
    if (ph == NULL) return 0;
    
    int count = 0;
    pnodeL2C current = ph;
    
    do {
        count++;
        current = current->pnext;
    } while (current != ph);
    
    return count;
}

// возвращает минимальный (min!=0) или максимальный (min==0) элемент в списке
double minmaxL2C(pnodeL2C ph, int min) {
    if (ph == NULL) return 0.0;
    
    double result = ph->data;
    pnodeL2C current = ph->pnext;
    
    while (current != ph) {
        if (min) {
            if (current->data < result) {
                result = current->data;
            }
        } else {
            if (current->data > result) {
                result = current->data;
            }
        }
        current = current->pnext;
    }
    
    return result;
}

// Вспомогательные структуры для поиска узла
typedef struct {
    double data;
    int above;
    pnodeL2C found;
    int first;
} SearchParams;

static int findNode(double nodeData) {
    static SearchParams *params = NULL;
    
    if (params == NULL) return 1;
    
    int condition;
    if (params->above) {
        condition = (nodeData > params->data);
    } else {
        condition = (nodeData < params->data);
    }
    
    if (condition) {
        params->found = (pnodeL2C)((char*)&nodeData - offsetof(tnodeL2C, data));
        if (params->first) {
            return 0; // Прерываем поиск при первом найденном
        }
    }
    
    return 1;
}

// возвращает первый (first!=0) или последний (first==0) элемент в списке
pnodeL2C abNodeL2C(pnodeL2C ph, int first, int above, double data) {
    if (ph == NULL) return NULL;
    
    SearchParams params;
    params.data = data;
    params.above = above;
    params.found = NULL;
    params.first = first;
    
    // Используем глобальную переменную для передачи параметров в findNode
    extern SearchParams *searchParams;
    searchParams = &params;
    
    listActionL2C(ph, 1, findNode);
    
    searchParams = NULL;
    
    return params.found;
}

// Глобальная переменная для передачи параметров в findNode
SearchParams *searchParams = NULL;

// Обновленная версия findNode с использованием глобальной переменной
static int findNodeUpdated(double nodeData) {
    if (searchParams == NULL) return 1;
    
    int condition;
    if (searchParams->above) {
        condition = (nodeData > searchParams->data);
    } else {
        condition = (nodeData < searchParams->data);
    }
    
    if (condition) {
        // Получаем указатель на узел из указателя на data
        pnodeL2C node = (pnodeL2C)((char*)&nodeData - offsetof(tnodeL2C, data));
        searchParams->found = node;
        if (searchParams->first) {
            return 0; // Прерываем поиск при первом найденном
        }
    }
    
    return 1;
}
