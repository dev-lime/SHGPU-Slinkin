// заголовочный файл для поддержки односвязных списков
#ifndef LISTUNIT_L1
#define LISTUNIT_L1

// структура для хранения узла списка
typedef struct tnodeL1 *pnodeL1; 
typedef struct tnodeL1
	{   char *data;
	    pnodeL1 pnext;
	} tnodeL1;

 // создает узел по копии переданной строки и возвращает его
 pnodeL1 createNodeL1(char *data);

 // вставляет узел в начало списка и возвращает вставленный узел 
 pnodeL1 addFirstNodeL1(pnodeL1 *ph, pnodeL1 p);
 // вставляет узел в конец списка и возвращает вставленный узел 
 pnodeL1 addLastNodeL1(pnodeL1 *ph, pnodeL1 p);
 // вставляет узел в список после переданного узла и возвращает вставленный узел 
 pnodeL1 insertAfterNodeL1(pnodeL1 pn, pnodeL1 p);

 // уничтожает узел и устанавливает его в NULL
 void disposeNodeL1(pnodeL1 *pn);
 // удаляет узел из списка после переданного узла и возвращает удаленный узел
 pnodeL1 deleteAfterNodeL1(pnodeL1 pn);
 // удаляет узел из списка после переданного узла и уничтожает удаленный узел
 void disposeAfterNodeL1(pnodeL1 pn);
 // уничтожает список и устанавливает его в NULL
 void disposeListL1(pnodeL1 *ph);
 
 // выполняет функцию func над каждым узлом списка
 // если func возвращает 0, listAction завершает перебор узлов списка  
 typedef int (*listfunc)(char*);
 void listActionL1(pnodeL1 ph, listfunc func);
 // выводит список в стандартный поток вывода
 void listOutL1(pnodeL1 ph);
 // возвращает количество элементов в списке
 int listCountL1(pnodeL1 ph);
 // Объединяет данные элементов списка в строку с использованием разделителя
 // delimiter между отдельными данными.
 // Полученную строку заносит в dest и возвращает в качестве результата функции.
 // Длина результирующей строки не должна превышать maxsize.
 // Если maxsize==0 или dest==NULL, функция возвращает NULL
 char *listSumStr(char *dest, int maxsize, pnodeL1 ph, char *delimiter);  

#endif // LISTUNIT_1

