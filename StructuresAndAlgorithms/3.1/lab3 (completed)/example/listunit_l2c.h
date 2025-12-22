// заголовочный файл для поддержки двусвязных кольцевых списков вещественных значений

#ifndef LISTUNIT_L2C
#define LISTUNIT_L2C

// структура для хранения узла списка
typedef struct tnodeL2C *pnodeL2C;
typedef struct
	{   double data;
	    pnodeL2C pprev,pnext;
	} tnodeL2C;

 // создает узел и возвращает его
 pnodeL2C createNodeL2C(double data);

 // вставляет узел в начало списка и возвращает вставленный узел 
 pnodeL2C addFirstNodeL2C(pnodeL2C *ph, pnodeL2C p);
 // вставляет узел в конец списка и возвращает вставленный узел 
 pnodeL2C addLastNodeL2C(pnodeL2C *ph, pnodeL2C p);
 // вставляет узел p в список после узла pn и возвращает вставленный узел 
 pnodeL2C insertAfterNodeL2C(pnodeL2C pn, pnodeL2C p);
 // вставляет узел p в список перед узлом pn и возвращает вставленный узел 
 pnodeL2C insertBeforeNodeL2C(pnodeL2C pn, pnodeL2C p);

 // удаляет узел из списка и возвращает удаленный узел
 pnodeL2C deleteNodeL2C(pnodeL2C *ph, pnodeL2C pn);
 // уничтожает узел и устанавливает его в NULL. 
 void disposeNodeL2C(pnodeL2C *pn);
 // уничтожает список и устанавливает его в NULL
 void disposeListL2C(pnodeL2C *ph);

 // выполняет функцию func над каждым узлом списка
 // если fwd!=0, перебор узлов осуществляется в прямом направлении, иначе - в обратном
 // если func возвращает 0, listAction завершает перебор узлов
 typedef int (*listfunc)(double);
 void listActionL2C(pnodeL2C ph, int fwd, listfunc func);

 // последующие функции должны использовать listActionL2C
 // выводит список в стандартный поток вывода в прямом (fwd!=0) или обратном (fwd==0) порядке
 void listOutL2C(pnodeL2C ph, int fwd);
 // возвращает количество элементов в списке
 int listCountL2C(pnodeL2C ph);
 // возвращает минимальный (min!=0) или максимальный (min==0) элемент в списке
 double minmaxL2C(pnodeL2C ph, int min);
 // возвращает первый (first!=0) или последний (first==0) элемент в списке, больший (above!=0)
 // или меньший (above==0) data
 pnodeL2C abNodeL2C(pnodeL2C ph, int first, int above, double data);

#endif // LISTUNIT_L2C

