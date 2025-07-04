#include <stdio.h>

int find_count = 0;

int line_find_one(const int src[], int src_size, int (*func)(int))
{
	find_count = 0;
	for (int i = 0; i < src_size; i++)
	{
		find_count++;
		if (func(src[i]) == 1)
		{
			return i;
		}
	}
	return -1;
}

int line_find_all(const int src[], int src_size, int (*func)(int),
				  int result[], int result_maxsize)
{
	find_count = 0;
	int count = 0;
	for (int i = 0; i < src_size && count < result_maxsize; i++)
	{
		find_count++;
		if (func(src[i]) == 1)
		{
			result[count++] = i;
		}
	}
	return count;
}

int bin_find_one(const int src[], int src_size, int (*func)(int))
{
	find_count = 0;
	int left = 0;
	int right = src_size - 1;

	while (left <= right)
	{
		find_count++;
		int mid = left + (right - left) / 2;
		int test_result = func(src[mid]);

		if (test_result == 0)
		{
			return mid;
		}
		else if (test_result < 0)
		{
			left = mid + 1;
		}
		else
		{
			right = mid - 1;
		}
	}
	return -1;
}

int bin_find_all(const int src[], int src_size, int (*func)(int),
				 int *res_beg, int *res_end)
{
	find_count = 0;

	int pos = bin_find_one(src, src_size, func);
	if (pos == -1)
	{
		return 0;
	}

	*res_beg = pos;
	while (*res_beg > 0 && func(src[*res_beg - 1]) == 0)
	{
		find_count++;
		(*res_beg)--;
	}

	*res_end = pos;
	while (*res_end < src_size - 1 && func(src[*res_end + 1]) == 0)
	{
		find_count++;
		(*res_end)++;
	}

	return *res_end - *res_beg + 1;
}

int test_equal(int value)
{
	return (value == 50) ? 0 : (value < 50 ? -1 : 1);
}

int test_equal_for_linear(int value)
{
	return (value == 50) ? 1 : 0;
}

int main()
{
	int arr[] = {10, 20, 30, 40, 50, 50, 50, 60, 70, 80, 90, 100};
	int size = sizeof(arr) / sizeof(arr[0]);

	printf("Линейный поиск одного элемента\n");
	int index = line_find_one(arr, size, test_equal_for_linear);
	printf("Индекс: %d\nИтераций: %d\n", index, find_count);

	printf("\nЛинейный поиск всех элементов\n");
	int results[10];
	int count = line_find_all(arr, size, test_equal_for_linear, results, 10);
	printf("Найдено %d элементов: ", count);
	for (int i = 0; i < count; i++)
	{
		printf("[%d] ", results[i]);
	}
	printf("\nИтераций: %d\n", find_count);

	printf("\nБинарный поиск одного элемента\n");
	index = bin_find_one(arr, size, test_equal);
	printf("Индекс: %d\nИтераций: %d\n", index, find_count);

	printf("\nБинарный поиск всех элементов\n");
	int beg, end;
	count = bin_find_all(arr, size, test_equal, &beg, &end);
	printf("Найдено %d элементов от [%d] до [%d]\nИтераций: %d\n",
		   count, beg, end, find_count);

	return 0;
}
