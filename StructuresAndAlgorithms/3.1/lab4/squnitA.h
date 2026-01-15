// заголовочный файл для поддержки стеков и очередей

#ifndef SQUNIT
#define SQUNIT

// структура для хранения стека целочисленных значений на базе массивов
typedef struct AStack *pAStack;
typedef struct AStack
	{   int *data;   // массив для хранения элементов стека
	    int maxsize; // максимальный размер массива
	    int count;   // количество элементов в массиве
	} AStack;

 // создает стек
 pAStack AStack_create(int maxsize);
 // уничтожает стек
 void AStack_destroy(pAStack stack);
 // помещает элемент на вершину стека
 void AStack_push(pAStack stack, int number);
 // возвращает элемент с вершины стека
 int AStack_pop(pAStack stack);
 // возвращает 1, если стек пустой, иначе - 0
 int AStack_empty(pAStack stack);
 // возвращает 1, если стек полон, иначе - 0
 int AStack_full(pAStack stack);
 // возвращает количество элементов в стеке
 int AStack_count(pAStack stack);
 

// структура для хранения очереди целочисленных значений на базе кольцевых массивов
typedef struct aQueue *paQueue;
typedef struct aQueue
	{   int *data;   // массив для хранения элементов очереди
	    int maxsize; // максимальный размер массива
	    int first,last;   // индексы начала и конца очереди
	} aQueue;

 // создает очередь
 paQueue aQueue_create(int maxsize);
 // уничтожает очередь
 void aQueue_destroy(paQueue que);
 // помещает элемент в конец очереди
 void aQueue_put(paQueue que, int number);
 // возвращает элемент из начала очереди
 int aQueue_get(paQueue que);
 // возвращает 1, если очередь пустая, иначе - 0
 int aQueue_empty(paQueue que);
 // возвращает 1, если очередь полностью заполнена, иначе - 0
 int aQueue_full(paQueue que);
 // возвращает количество элементов в очереди
 int aQueue_count(paQueue que);


#endif // SQUNIT
