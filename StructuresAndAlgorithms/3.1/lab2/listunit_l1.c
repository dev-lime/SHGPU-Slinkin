#include "listunit_l1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

pnodeL1 createNodeL1(char *data)
{
    if (data == NULL) return NULL;
    
    pnodeL1 newNode = (pnodeL1)malloc(sizeof(tnodeL1));
    if (newNode == NULL) return NULL;
    
    (*newNode).data = (char*)malloc(strlen(data) + 1);
    if ((*newNode).data == NULL) {
        free(newNode);
        return NULL;
    }
    
    strcpy((*newNode).data, data);
    (*newNode).pnext = NULL;
    
    return newNode;
}

pnodeL1 addFirstNodeL1(pnodeL1 *ph, pnodeL1 p)
{
    if (p == NULL) return NULL;
    
    (*p).pnext = *ph;
    *ph = p;
    
    return p;
}

pnodeL1 addLastNodeL1(pnodeL1 *ph, pnodeL1 p)
{
    if (p == NULL) return NULL;
    
    if (*ph == NULL) {
        *ph = p;
        return p;
    }
    
    pnodeL1 current = *ph;
    while ((*current).pnext != NULL) {
        current = (*current).pnext;
    }
    
    (*current).pnext = p;
    (*p).pnext = NULL;
    
    return p;
}

pnodeL1 insertAfterNodeL1(pnodeL1 pn, pnodeL1 p)
{
    if (pn == NULL || p == NULL) return NULL;
    
    (*p).pnext = (*pn).pnext;
    (*pn).pnext = p;
    
    return p;
}

void disposeNodeL1(pnodeL1 *pn)
{
    if (pn == NULL || *pn == NULL) return;
    
    free((*(*pn)).data);
    free(*pn);
    *pn = NULL;
}

pnodeL1 deleteAfterNodeL1(pnodeL1 pn)
{
    if (pn == NULL || (*pn).pnext == NULL) return NULL;
    
    pnodeL1 deleted = (*pn).pnext;
    (*pn).pnext = (*deleted).pnext;
    (*deleted).pnext = NULL;
    
    return deleted;
}

void disposeAfterNodeL1(pnodeL1 pn)
{
    pnodeL1 deleted = deleteAfterNodeL1(pn);
    if (deleted != NULL) {
        disposeNodeL1(&deleted);
    }
}

void disposeListL1(pnodeL1 *ph)
{
    if (ph == NULL) return;
    
    pnodeL1 current = *ph;
    while (current != NULL) {
        pnodeL1 next = (*current).pnext;
        disposeNodeL1(&current);
        current = next;
    }
    
    *ph = NULL;
}

void listActionL1(pnodeL1 ph, listfunc func)
{
    if (func == NULL) return;
    
    pnodeL1 current = ph;
    while (current != NULL) {
        if (func((*current).data) == 0) {
            break;
        }
        current = (*current).pnext;
    }
}

void listOutL1(pnodeL1 ph)
{
    pnodeL1 current = ph;
    printf("List: ");
    while (current != NULL) {
        printf("%s", (*current).data);
        if ((*current).pnext != NULL) {
            printf(" > ");
        }
        current = (*current).pnext;
    }
    printf("\n");
}

int listCountL1(pnodeL1 ph)
{
    int count = 0;
    pnodeL1 current = ph;
    
    while (current != NULL) {
        count++;
        current = (*current).pnext;
    }
    
    return count;
}

#include "listunit_l1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

pnodeL1 createNodeL1(char *data)
{
    if (data == NULL) return NULL;
    
    pnodeL1 newNode = (pnodeL1)malloc(sizeof(tnodeL1));
    if (newNode == NULL) return NULL;
    
    (*newNode).data = (char*)malloc(strlen(data) + 1);
    if ((*newNode).data == NULL) {
        free(newNode);
        return NULL;
    }
    
    strcpy((*newNode).data, data);
    (*newNode).pnext = NULL;
    
    return newNode;
}

pnodeL1 addFirstNodeL1(pnodeL1 *ph, pnodeL1 p)
{
    if (p == NULL) return NULL;
    
    (*p).pnext = *ph;
    *ph = p;
    
    return p;
}

pnodeL1 addLastNodeL1(pnodeL1 *ph, pnodeL1 p)
{
    if (p == NULL) return NULL;
    
    if (*ph == NULL) {
        *ph = p;
        return p;
    }
    
    pnodeL1 current = *ph;
    while ((*current).pnext != NULL) {
        current = (*current).pnext;
    }
    
    (*current).pnext = p;
    (*p).pnext = NULL;
    
    return p;
}

pnodeL1 insertAfterNodeL1(pnodeL1 pn, pnodeL1 p)
{
    if (pn == NULL || p == NULL) return NULL;
    
    (*p).pnext = (*pn).pnext;
    (*pn).pnext = p;
    
    return p;
}

void disposeNodeL1(pnodeL1 *pn)
{
    if (pn == NULL || *pn == NULL) return;
    
    free((*(*pn)).data);
    free(*pn);
    *pn = NULL;
}

pnodeL1 deleteAfterNodeL1(pnodeL1 pn)
{
    if (pn == NULL || (*pn).pnext == NULL) return NULL;
    
    pnodeL1 deleted = (*pn).pnext;
    (*pn).pnext = (*deleted).pnext;
    (*deleted).pnext = NULL;
    
    return deleted;
}

void disposeAfterNodeL1(pnodeL1 pn)
{
    pnodeL1 deleted = deleteAfterNodeL1(pn);
    if (deleted != NULL) {
        disposeNodeL1(&deleted);
    }
}

void disposeListL1(pnodeL1 *ph)
{
    if (ph == NULL) return;
    
    pnodeL1 current = *ph;
    while (current != NULL) {
        pnodeL1 next = (*current).pnext;
        disposeNodeL1(&current);
        current = next;
    }
    
    *ph = NULL;
}

void listActionL1(pnodeL1 ph, listfunc func)
{
    if (func == NULL) return;
    
    pnodeL1 current = ph;
    while (current != NULL) {
        if (func((*current).data) == 0) {
            break;
        }
        current = (*current).pnext;
    }
}

void listOutL1(pnodeL1 ph)
{
    pnodeL1 current = ph;
    printf("List: ");
    while (current != NULL) {
        printf("%s", (*current).data);
        if ((*current).pnext != NULL) {
            printf(" > ");
        }
        current = (*current).pnext;
    }
    printf("\n");
}

int listCountL1(pnodeL1 ph)
{
    int count = 0;
    pnodeL1 current = ph;
    
    while (current != NULL) {
        count++;
        current = (*current).pnext;
    }
    
    return count;
}

char *listSumStr(char *dest, int maxsize, pnodeL1 ph, char *delimiter)
{
    if (dest == NULL || maxsize <= 0) return NULL;
    
    dest[0] = '\0';
    pnodeL1 current = ph;
    int remaining = maxsize - 1;
    
    while (current != NULL && remaining > 0) {
        int data_len = strlen((*current).data);
        if (data_len > remaining) {
            strncat(dest, (*current).data, remaining);
            remaining = 0;
            break;
        }
        
        strncat(dest, (*current).data, remaining);
        remaining -= data_len;
        
        if ((*current).pnext != NULL && delimiter != NULL && remaining > 0) {
            int delim_len = strlen(delimiter);
            if (delim_len > remaining) {
                strncat(dest, delimiter, remaining);
                remaining = 0;
                break;
            }
            
            strncat(dest, delimiter, remaining);
            remaining -= delim_len;
        }
        
        current = (*current).pnext;
    }
    
    return dest;
}
