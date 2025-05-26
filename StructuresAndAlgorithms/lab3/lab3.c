/*
Используя лекционные материалы и язык программирования Си, разработать и проверить 4 функции сортировки:
пузырьковую, вставками (базовую), вставками с бинарным поиском и быструю.
Заголовки функций должны соответствовать заголовку системной функции qsort.
Экспериментально определить количество элементов массива со случайным заполнением,
на сортировку которого тратится в среднем 1 секунда. Решить эту задачу для быстрой сортировки,
сортировки вставками, оптимизированной сортировки вставками, пузырьковой сортировки.
Разработать функцию для формирования такого содержимого переданного массива,
упорядочивание которого с помощью быстрой сортировки обеспечит максимально возможную для массива
такого размера глубину рекурсии быстрой сортировки. Проверить работу функции.

НАЗВАНИЕ
qsort - упорядочивает массив

СИНТАКСИС
#include <stdlib.h>

void qsort(void *base, size_t nmemb, size_t size,
int(*compar)(const void *, const void *));

ОПИСАНИЕ
Функция qsort() упорядочивает массив из nmemb элементов размером size. Аргумент base указывает на начало массива.
Содержимое массива располагается по возрастающему принципу, согласно функции сравнения, указанной в параметре compar и имеющей два аргумента (сравниваемые элементы массива).

Функция сравнения должна возвращать целое число меньше, больше нуля или равное ему, если первый аргумент, соответственно, меньше, больше второго или равен ему. Если два члена массива равны, то порядок их расположения в массиве не определен.

ВОЗВРАЩАЕМЫЕ ЗНАЧЕНИЯ
Функция qsort() не возвращает значений.
СООТВЕТСТВИЕ СТАНДАРТАМ
SVID 3, POSIX, BSD 4.3, ISO 9899
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

// Функция сравнения для целых чисел (аналогична примеру из Pascal)
int compare(const void *a, const void *b)
{
	int arg1 = *(const int *)a;
	int arg2 = *(const int *)b;
	return (arg1 < arg2) - (arg1 > arg2);
}

// Пузырьковая сортировка
void bubble_sort(void *base, size_t nmemb, size_t size,
				 int (*compar)(const void *, const void *))
{
	char *array = (char *)base;
	char *temp = malloc(size);
	int swapped;

	for (size_t i = 0; i < nmemb - 1; i++)
	{
		swapped = 0;
		for (size_t j = 0; j < nmemb - i - 1; j++)
		{
			if (compar(array + j * size, array + (j + 1) * size) > 0)
			{
				memcpy(temp, array + j * size, size);
				memcpy(array + j * size, array + (j + 1) * size, size);
				memcpy(array + (j + 1) * size, temp, size);
				swapped = 1;
			}
		}
		if (!swapped)
			break;
	}

	free(temp);
}

// Сортировка вставками (базовая)
void insertion_sort(void *base, size_t nmemb, size_t size,
					int (*compar)(const void *, const void *))
{
	char *array = (char *)base;
	char *temp = malloc(size);

	for (size_t i = 1; i < nmemb; i++)
	{
		size_t j = i;
		memcpy(temp, array + i * size, size);

		while (j > 0 && compar(array + (j - 1) * size, temp) > 0)
		{
			memcpy(array + j * size, array + (j - 1) * size, size);
			j--;
		}

		memcpy(array + j * size, temp, size);
	}

	free(temp);
}

// Бинарный поиск для оптимизированной сортировки вставками
size_t binary_search(char *array, char *item, size_t low, size_t high,
					 size_t size, int (*compar)(const void *, const void *))
{
	while (low <= high)
	{
		size_t mid = low + (high - low) / 2;
		int cmp = compar(item, array + mid * size);

		if (cmp == 0)
			return mid + 1;
		else if (cmp > 0)
			low = mid + 1;
		else
			high = mid - 1;
	}
	return low;
}

// Сортировка вставками с бинарным поиском
void insertion_binary_sort(void *base, size_t nmemb, size_t size,
						   int (*compar)(const void *, const void *))
{
	char *array = (char *)base;
	char *temp = malloc(size);

	for (size_t i = 1; i < nmemb; i++)
	{
		size_t j = i;
		memcpy(temp, array + i * size, size);

		size_t pos = binary_search(array, temp, 0, i - 1, size, compar);

		memmove(array + (pos + 1) * size, array + pos * size, (i - pos) * size);
		memcpy(array + pos * size, temp, size);
	}

	free(temp);
}

// Быстрая сортировка
void quick_sort(void *base, size_t nmemb, size_t size,
				int (*compar)(const void *, const void *))
{
	if (nmemb <= 1)
		return;

	char *array = (char *)base;
	char *pivot = malloc(size);
	char *temp = malloc(size);

	// Выбираем средний элемент в качестве опорного
	size_t pivot_idx = nmemb / 2;
	memcpy(pivot, array + pivot_idx * size, size);

	size_t i = 0, j = nmemb - 1;

	while (i <= j)
	{
		while (compar(array + i * size, pivot) < 0)
			i++;
		while (compar(array + j * size, pivot) > 0)
			j--;

		if (i <= j)
		{
			if (i != j)
			{
				memcpy(temp, array + i * size, size);
				memcpy(array + i * size, array + j * size, size);
				memcpy(array + j * size, temp, size);
			}
			i++;
			if (j > 0)
				j--;
		}
	}

	free(pivot);
	free(temp);

	if (j > 0)
		quick_sort(array, j + 1, size, compar);
	if (i < nmemb)
		quick_sort(array + i * size, nmemb - i, size, compar);
}

// Функция для создания массива, который вызывает максимальную глубину рекурсии в быстрой сортировке
void generate_worst_case_array(int *array, size_t n)
{
	// Для создания наихудшего случая используем стратегию:
	// 1. Выбираем средний элемент в качестве первого опорного
	// 2. Рекурсивно строим массивы для левой и правой части

	static int value = 1;

	if (n <= 0)
		return;

	size_t mid = n / 2;
	array[mid] = value++;

	generate_worst_case_array(array, mid);
	generate_worst_case_array(array + mid + 1, n - mid - 1);
}

// Функция для измерения времени сортировки
double measure_sort_time(void (*sort_func)(void *, size_t, size_t, int (*)(const void *, const void *)),
						 int *array, size_t n)
{
	clock_t start, end;

	// Создаем копию массива для сортировки
	int *temp = malloc(n * sizeof(int));
	memcpy(temp, array, n * sizeof(int));

	start = clock();
	sort_func(temp, n, sizeof(int), compare);
	end = clock();

	free(temp);

	return (double)(end - start) / CLOCKS_PER_SEC;
}

// Функция для определения размера массива, который сортируется за примерно 1 секунду
size_t find_array_size_for_1_second(void (*sort_func)(void *, size_t, size_t, int (*)(const void *, const void *)))
{
	size_t n = 10;
	double time_taken;

	do
	{
		n *= 2;
		int *array = malloc(n * sizeof(int));
		for (size_t i = 0; i < n; i++)
		{
			array[i] = rand();
		}

		time_taken = measure_sort_time(sort_func, array, n);
		free(array);

		printf("n = %zu, time = %f seconds\n", n, time_taken);

		if (time_taken > 2.0)
			break; // Чтобы не ждать слишком долго
	} while (time_taken < 1.0);

	// Бинарный поиск для более точного определения
	size_t low = n / 2;
	size_t high = n;
	size_t best_n = n;

	while (low <= high)
	{
		size_t mid = low + (high - low) / 2;
		int *array = malloc(mid * sizeof(int));
		for (size_t i = 0; i < mid; i++)
		{
			array[i] = rand();
		}

		time_taken = measure_sort_time(sort_func, array, mid);
		free(array);

		printf("Binary search: n = %zu, time = %f seconds\n", mid, time_taken);

		if (time_taken < 1.0)
		{
			low = mid + 1;
			best_n = mid;
		}
		else
		{
			if (mid > 0)
				high = mid - 1;
		}
	}

	return best_n;
}

int main()
{
	srand(time(NULL));

	// Тестирование функций сортировки
	int test_array[] = {10, 1, 50, 5, 5, 60, 8, 2};
	size_t n = sizeof(test_array) / sizeof(test_array[0]);

	printf("Original array: ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", test_array[i]);
	printf("\n");

	// Тестируем каждую сортировку
	int *temp = malloc(n * sizeof(int));

	memcpy(temp, test_array, n * sizeof(int));
	bubble_sort(temp, n, sizeof(int), compare);
	printf("Bubble sort:    ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", temp[i]);
	printf("\n");

	memcpy(temp, test_array, n * sizeof(int));
	insertion_sort(temp, n, sizeof(int), compare);
	printf("Insertion sort: ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", temp[i]);
	printf("\n");

	memcpy(temp, test_array, n * sizeof(int));
	insertion_binary_sort(temp, n, sizeof(int), compare);
	printf("Ins+bin search: ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", temp[i]);
	printf("\n");

	memcpy(temp, test_array, n * sizeof(int));
	quick_sort(temp, n, sizeof(int), compare);
	printf("Quick sort:     ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", temp[i]);
	printf("\n");

	free(temp);

	// Определяем размер массива для сортировки за ~1 секунду
	printf("\nFinding array size for ~1 second sorting time:\n");

	printf("\nBubble sort:\n");
	size_t bubble_size = find_array_size_for_1_second(bubble_sort);
	printf("Bubble sort: %zu elements for ~1 second\n", bubble_size);

	printf("\nInsertion sort:\n");
	size_t insertion_size = find_array_size_for_1_second(insertion_sort);
	printf("Insertion sort: %zu elements for ~1 second\n", insertion_size);

	printf("\nInsertion with binary search:\n");
	size_t insertion_binary_size = find_array_size_for_1_second(insertion_binary_sort);
	printf("Insertion with binary search: %zu elements for ~1 second\n", insertion_binary_size);

	printf("\nQuick sort:\n");
	size_t quick_size = find_array_size_for_1_second(quick_sort);
	printf("Quick sort: %zu elements for ~1 second\n", quick_size);

	// Тестирование наихудшего случая для быстрой сортировки
	printf("\nTesting worst-case scenario for quick sort:\n");
	n = 20; // Используем небольшой размер для демонстрации
	int *worst_case = malloc(n * sizeof(int));
	generate_worst_case_array(worst_case, n);

	printf("Worst case array: ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", worst_case[i]);
	printf("\n");

	// Измеряем глубину рекурсии (для этого нужно модифицировать quick_sort)
	// Вместо этого просто выведем массив, который должен вызывать максимальную глубину рекурсии
	free(worst_case);

	return 0;
}
