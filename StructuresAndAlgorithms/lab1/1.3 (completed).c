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

void Fill(int x, int y)
{
	AddDepth();

	if (!IsCellEmpty(x, y))
	{
		SubDepth();
		return;
	}

	FillCell(x, y);

	// Заполнение вправо
	int xRight = x;
	while (IsCellEmpty(xRight + 1, y))
	{
		xRight++;
		FillCell(xRight, y);
	}

	// Заполнение влево
	int xLeft = x;
	while (IsCellEmpty(xLeft - 1, y))
	{
		xLeft--;
		FillCell(xLeft, y);
	}

	// Рекурсия по вертикали в диапазоне [xLeft, xRight]
	for (int x1 = xLeft; x1 <= xRight; x1++)
	{
		if (IsCellEmpty(x1, y + 1))
			Fill(x1, y + 1); // вниз
		if (IsCellEmpty(x1, y - 1))
			Fill(x1, y - 1); // вверх
	}

	SubDepth();
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
