/*
Разработать модуль для поддержки двусвязных кольцевых списков, заголовочный файл listunit_l2с.h прилагается.
Создать программу/набор программ для проверки работоспособности всех функций модуля.
*/
/*

// Terminal:
> make clean && make && ./main

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "listunit_l2c.h"

// функция для listAction: печатает значения >=15, прерывается при <15
int printTo(double d)
{
    if (d < 15) return 0;
    printf("|%lf| ", d);
    return 1;
}

int main()
{
    printf("----------------------\n");
    pnodeL2C tn = NULL, tn1 = NULL;
    
    // создание списка
    addFirstNodeL2C(&tn, createNodeL2C(40));
    listOutL2C(tn, 1);
    printf("Количество: %d\n", listCountL2C(tn));
    
    // вставка после и перед
    insertAfterNodeL2C(tn, createNodeL2C(20));
    insertBeforeNodeL2C(tn, createNodeL2C(10));
    listOutL2C(tn, 1);
    listOutL2C(tn, 0);
    printf("Количество: %d\n", listCountL2C(tn));
    
    printf("-----------------\n");
    
    // поиск мин/макс
    printf("Макс: %lf, Мин: %lf\n", 
           minmaxL2C(tn, 0), minmaxL2C(tn, 1));
    
    // abNodeL2C с разными параметрами
    printf("abNode тесты:\n");
    printf("Последний <25: %lf\n", abNodeL2C(tn, 0, 0, 25)->data);
    printf("Первый >35: %lf\n", abNodeL2C(tn, 1, 1, 35)->data);
    
    // listAction
    printf("\nlistAction (прямой обход):\n");
    listActionL2C(tn, 1, &printTo);
    listActionL2C(tn, 0, &printTo);
    printf("\n");
    
    // удаление узлов
    printf("\nУдаление элементов:\n");
    
    // удаляет элемент >35 (это 40)
    tn1 = deleteNodeL2C(&tn, abNodeL2C(tn, 1, 1, 35));
    disposeListL2C(&tn1);
    listOutL2C(tn, 1);
    listOutL2C(tn, 0);
    
    // удаляет элемент <15 (это 10)
    tn1 = deleteNodeL2C(&tn, abNodeL2C(tn, 1, 0, 15));
    disposeListL2C(&tn1);
    listOutL2C(tn, 1);
    listOutL2C(tn, 0);
    
    // удаляет оставшийся элемент (20)
    tn1 = deleteNodeL2C(&tn, tn);
    disposeListL2C(&tn1);
    listOutL2C(tn, 1);
    listOutL2C(tn, 0);
    
    // очистка
    disposeListL2C(&tn);
    
    return 0;
}
