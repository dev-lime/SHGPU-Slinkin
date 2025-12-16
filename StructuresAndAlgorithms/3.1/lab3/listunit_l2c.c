#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "listunit_l2c.h"

// вспомогательные функции для listActionL2C
static int printFunction(double data)
{
    printf("%.2f ", data);
    return 1;
}

static int countFunction(double data)
{
    (void)data;
    return 1;
}

static int testFunctionWrapper(double data)
{
    static int count = 0;
    printf("Элемент %d: %.2f\n", ++count, data);
    if (count >= 3) return 0;
    return 1;
}

// создает узел и возвращает его
pnodeL2C createNodeL2C(double data)
{
    pnodeL2C newNode = (pnodeL2C)malloc(sizeof(tnodeL2C));
    if (newNode)
    {
        (*newNode).data = data;
        (*newNode).pprev = NULL;
        (*newNode).pnext = NULL;
    }
    return newNode;
}

// вставляет узел в начало списка и возвращает вставленный узел
pnodeL2C addFirstNodeL2C(pnodeL2C *ph, pnodeL2C p)
{
    if (!p) return NULL;
    
    if (!(*ph))
    {
        // пустой список
        (*p).pnext = p;
        (*p).pprev = p;
        *ph = p;
    }
    else
    {
        // вставляем перед первым узлом
        pnodeL2C last = (*(*ph)).pprev;
        (*p).pnext = *ph;
        (*p).pprev = last;
        (*(*ph)).pprev = p;
        (*last).pnext = p;
        *ph = p;
    }
    return p;
}

// вставляет узел в конец списка и возвращает вставленный узел
pnodeL2C addLastNodeL2C(pnodeL2C *ph, pnodeL2C p)
{
    if (!p) return NULL;
    
    if (!(*ph))
    {
        // пустой список
        (*p).pnext = p;
        (*p).pprev = p;
        *ph = p;
    }
    else
    {
        // вставляем после последнего узла
        pnodeL2C last = (*(*ph)).pprev;
        (*p).pnext = *ph;
        (*p).pprev = last;
        (*last).pnext = p;
        (*(*ph)).pprev = p;
    }
    return p;
}

// вставляет узел p в список после узла pn и возвращает вставленный узел
pnodeL2C insertAfterNodeL2C(pnodeL2C pn, pnodeL2C p)
{
    if (!pn || !p) return NULL;
    
    (*p).pnext = (*pn).pnext;
    (*p).pprev = pn;
    (*(*pn).pnext).pprev = p;
    (*pn).pnext = p;
    
    return p;
}

// вставляет узел p в список перед узлом pn и возвращает вставленный узел
pnodeL2C insertBeforeNodeL2C(pnodeL2C pn, pnodeL2C p)
{
    if (!pn || !p) return NULL;
    
    (*p).pprev = (*pn).pprev;
    (*p).pnext = pn;
    (*(*pn).pprev).pnext = p;
    (*pn).pprev = p;
    
    return p;
}

// удаляет узел из списка и возвращает удаленный узел
pnodeL2C deleteNodeL2C(pnodeL2C *ph, pnodeL2C pn)
{
    if (!pn || !ph) return NULL;
    
    if ((*pn).pnext == pn) // единственный узел в списке
    {
        *ph = NULL;
    }
    else
    {
        (*(*pn).pprev).pnext = (*pn).pnext;
        (*(*pn).pnext).pprev = (*pn).pprev;
        
        if (*ph == pn) // удаляем голову списка
        {
            *ph = (*pn).pnext;
        }
    }
    
    // отвязываем узел
    (*pn).pnext = NULL;
    (*pn).pprev = NULL;
    
    return pn;
}

// уничтожает узел и устанавливает его в NULL
void disposeNodeL2C(pnodeL2C *pn)
{
    if (pn && *pn)
    {
        free(*pn);
        *pn = NULL;
    }
}

// уничтожает список и устанавливает его в NULL
void disposeListL2C(pnodeL2C *ph)
{
    if (!ph || !(*ph)) return;
    
    pnodeL2C current = *ph;
    pnodeL2C next;
    
    if ((*current).pnext == NULL || (*current).pprev == NULL)
    {
        // одиночный узел
        free(current);
        *ph = NULL;
        return;
    }
    
    pnodeL2C start = current;
    
    do
    {
        next = (*current).pnext;
        free(current);
        current = next;
    } while (current != start);
    
    *ph = NULL;
}

// выполняет функцию func над каждым узлом списка
void listActionL2C(pnodeL2C ph, int fwd, listfunc func)
{
    if (!ph || !func) return;
    
    pnodeL2C current = ph;
    pnodeL2C start = ph;
    
    do
    {
        if (!func((*current).data)) break;
        
        if (fwd)
            current = (*current).pnext;
        else
            current = (*current).pprev;
            
    } while (current != start);
}

// выводит список в стандартный поток вывода
void listOutL2C(pnodeL2C ph, int fwd)
{
    if (!ph)
    {
        printf("Пустой список\n");
        return;
    }
    
    printf("Список (%s): ", fwd ? "прямой" : "обратный");
    listActionL2C(ph, fwd, printFunction);
    printf("\n");
}

// возвращает количество элементов в списке
int listCountL2C(pnodeL2C ph)
{
    if (!ph) return 0;
    
    int count = 0;
    pnodeL2C current = ph;
    
    do
    {
        count++;
        current = (*current).pnext;
    } while (current != ph);
    
    return count;
}

// возвращает минимальный (min!=0) или максимальный (min==0) элемент в списке
double minmaxL2C(pnodeL2C ph, int min)
{
    if (!ph) return 0.0;
    
    double result = (*ph).data;
    pnodeL2C current = (*ph).pnext;
    
    while (current != ph)
    {
        if (min)
        {
            if ((*current).data < result)
                result = (*current).data;
        }
        else
        {
            if ((*current).data > result)
                result = (*current).data;
        }
        current = (*current).pnext;
    }
    
    return result;
}

// возвращает первый (first!=0) или последний (first==0) элемент в списке
pnodeL2C abNodeL2C(pnodeL2C ph, int first, int above, double data)
{
    if (!ph) return NULL;
    
    pnodeL2C result = NULL;
    pnodeL2C current = ph;
    
    do
    {
        int condition = above ? ((*current).data > data) : ((*current).data < data);
        
        if (condition)
        {
            if (first)
                return current;
            else
                result = current;
        }
        
        current = (*current).pnext;
    } while (current != ph);
    
    return result;
}
