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

/*
Замер времени с выводом;
Сравнение через diff больших данных между qsort и моей сортировкой
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

int compare(const void *a, const void *b)
{
	int arg1 = *(const int *)a;
	int arg2 = *(const int *)b;
	return (arg1 < arg2) - (arg1 > arg2);
}

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

size_t binary_search(char *array, char *item, size_t low, size_t high,
					 size_t size, int (*compar)(const void *, const void *))
{
	while (low < high)
	{
		size_t mid = low + (high - low) / 2;
		int cmp = compar(item, array + mid * size);

		if (cmp == 0)
			return mid + 1;
		else if (cmp > 0)
			low = mid + 1;
		else
			high = mid;
	}
	return (low == high && compar(item, array + low * size) > 0) ? low + 1 : low;
}

void insertion_binary_sort(void *base, size_t nmemb, size_t size,
						   int (*compar)(const void *, const void *))
{
	if (nmemb <= 1)
		return;

	char *array = (char *)base;
	char *temp = malloc(size);
	if (!temp)
		return;

	for (size_t i = 1; i < nmemb; i++)
	{
		memcpy(temp, array + i * size, size);

		size_t pos = binary_search(array, temp, 0, i - 1, size, compar);

		if (pos < i)
		{
			memmove(array + (pos + 1) * size, array + pos * size, (i - pos) * size);
			memcpy(array + pos * size, temp, size);
		}
	}

	free(temp);
}

void quick_sort(void *base, size_t nmemb, size_t size,
				int (*compar)(const void *, const void *))
{
	if (nmemb <= 1)
		return;

	char *array = (char *)base;
	char *pivot = malloc(size);
	char *temp = malloc(size);

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

int main()
{
	int test_array[] = {10, 1, 50, 5, 5, 60, 8, 2};
	size_t n = sizeof(test_array) / sizeof(test_array[0]);

	printf("Original array: ");
	for (size_t i = 0; i < n; i++)
		printf("%d ", test_array[i]);
	printf("\n");

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

	return 0;
}
