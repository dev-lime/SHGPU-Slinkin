/*
Модифицировать программу закраски ограниченной области,
преобразовав хвостовую(концевую) рекурсию в цикл (сокращение 1 рекурсивного вызова).
Модифицировать полученную программу, заменив рекурсию по горизонтали циклом (сокращение 2 рекурсивных вызовов).
Оценить максимальную глубину рекурсии для 3-х версий алгоритма закраски ограниченной области:
1) исходной,
2) с сокращением 1 рекурсивного вызова,
3) с сокращением 2-х рекурсивных вызовов.
*/

#include <stdio.h>
#include <stdbool.h>

#define MAX 10
#define MIN 1

int Depth = 0;
int MaxDepth = 0;

char a[MAX][MAX + 1] = {
	"0001001000",
	"0010010000",
	"0001010111",
	"0000001100",
	"0001100000",
	"0000101111",
	"0000101000",
	"0000101000",
	"0001101100",
	"0010000100"};

void FillCell(int x, int y)
{
	a[y - 1][x - 1] = '*';
}

void AddDepth()
{
	Depth++;
	if (Depth > MaxDepth)
		MaxDepth = Depth;
}

void SubDepth()
{
	Depth--;
}

bool IsCellEmpty(int x, int y)
{
	if (x < MIN || x > MAX || y < MIN || y > MAX || a[y - 1][x - 1] != '0')
		return false;
	return true;
}

typedef struct
{
	int x;
	int y;
} Point;

Point stack[MAX * MAX];
int stackTop = -1;

void Push(int x, int y)
{
	stackTop++;
	stack[stackTop].x = x;
	stack[stackTop].y = y;
}

Point Pop()
{
	return stack[stackTop--];
}

bool StackIsEmpty()
{
	return stackTop == -1;
}

void Fill(int startX, int startY)
{
	Push(startX, startY);

	while (!StackIsEmpty())
	{
		AddDepth();
		Point p = Pop();
		int x = p.x;
		int y = p.y;

		if (!IsCellEmpty(x, y))
		{
			SubDepth();
			continue;
		}

		FillCell(x, y);

		// Горизонтальное заполнение (цикл вправо)
		int right = x + 1;
		while (right <= MAX && IsCellEmpty(right, y))
		{
			FillCell(right, y);
			if (IsCellEmpty(right, y + 1))
				Push(right, y + 1);
			if (IsCellEmpty(right, y - 1))
				Push(right, y - 1);
			right++;
		}

		// Горизонтальное заполнение (цикл влево)
		int left = x - 1;
		while (left >= MIN && IsCellEmpty(left, y))
		{
			FillCell(left, y);
			if (IsCellEmpty(left, y + 1))
				Push(left, y + 1);
			if (IsCellEmpty(left, y - 1))
				Push(left, y - 1);
			left--;
		}

		// Вертикальные направления (добавляем в стек)
		if (IsCellEmpty(x, y + 1))
			Push(x, y + 1);
		if (IsCellEmpty(x, y - 1))
			Push(x, y - 1);

		SubDepth();
	}
}

void PrintArray()
{
	for (int i = 0; i < MAX; i++)
	{
		printf("%s\n", a[i]);
	}
}

int main()
{
	Fill(5, 1);
	PrintArray();
	printf("\nMax recursion depth = %d\n", MaxDepth);
	return 0;
}
