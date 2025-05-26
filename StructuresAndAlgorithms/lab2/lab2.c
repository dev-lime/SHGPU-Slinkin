#include "findunit.h"

int find_count = 0;

int line_find_one(const int src[], int src_size, testfunc func)
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

int line_find_all(const int src[], int src_size, testfunc func,
				  int result[], int result_maxsize)
{
	find_count = 0;
	int found = 0;

	for (int i = 0; i < src_size && found < result_maxsize; i++)
	{
		find_count++;
		if (func(src[i]) == 1)
		{
			result[found++] = i;
		}
	}

	return found;
}

int bin_find_one(const int src[], int src_size, testfunc func)
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

int bin_find_all(const int src[], int src_size, testfunc func,
				 int *res_beg, int *res_end)
{
	find_count = 0;

	// First find one occurrence
	int pos = bin_find_one(src, src_size, func);
	if (pos == -1)
	{
		*res_beg = -1;
		*res_end = -1;
		return 0;
	}

	// Search for the beginning of the range
	int left = 0;
	int right = pos;
	int beg = pos;
	while (left <= right)
	{
		find_count++;
		int mid = left + (right - left) / 2;
		if (func(src[mid]) == 0)
		{
			beg = mid;
			right = mid - 1;
		}
		else
		{
			left = mid + 1;
		}
	}

	// Search for the end of the range
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

	*res_beg = beg;
	*res_end = end;
	return end - beg + 1;
}
