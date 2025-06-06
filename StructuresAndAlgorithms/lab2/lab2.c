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
			right = mid - 1;
		}
		else
		{
			left = mid + 1;
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

	int left = 0;
	int right = pos;
	int start = pos;
	while (left <= right)
	{
		find_count++;
		int mid = left + (right - left) / 2;
		if (func(src[mid]) == 0)
		{
			start = mid;
			right = mid - 1;
		}
		else
		{
			left = mid + 1;
		}
	}

	left = pos;
	right = src_size - 1;
	int end = pos;
	while (left <= right)
	{
		find_count++;
		int mid = left + (right - left) / 2;
		if (func(src[mid]) == 0)
		{
			end = mid;
			left = mid + 1;
		}
		else
		{
			right = mid - 1;
		}
	}

	*res_beg = start;
	*res_end = end;
	return end - start + 1;
}

int test_equal(int target, int value)
{
	return (value == target) ? 0 : (value < target ? -1 : 1);
}

int test_greater(int value)
{
	return (value > 50) ? 1 : 0;
}

int main()
{
	int arr[] = {10, 20, 30, 40, 50, 50, 50, 60, 70, 80, 90, 100};
	int size = sizeof(arr) / sizeof(arr[0]);

	printf("Linear search for first element > 50:\n");
	int index = line_find_one(arr, size, test_greater);
	printf("Found at index: %d, iterations: %d\n", index, find_count);

	printf("\nLinear search for all elements > 50:\n");
	int results[10];
	int count = line_find_all(arr, size, test_greater, results, 10);
	printf("Found %d elements: ", count);
	for (int i = 0; i < count; i++)
	{
		printf("%d ", results[i]);
	}
	printf(", iterations: %d\n", find_count);

	printf("\nBinary search for 50:\n");
	int (*test_equal_50)(int) = &test_equal;
	index = bin_find_one(arr, size, test_equal_50);
	printf("Found at index: %d, iterations: %d\n", index, find_count);

	printf("\nBinary search for all 50s:\n");
	int beg, end;
	count = bin_find_all(arr, size, test_equal_50, &beg, &end);
	printf("Found %d elements from index %d to %d, iterations: %d\n",
		   count, beg, end, find_count);

	return 0;
}
