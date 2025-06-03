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

	while (true)
	{

		if (!IsCellEmpty(x, y))
		{
			SubDepth();
			return;
		}

		FillCell(x, y);

		Fill(x + 1, y); // вправо
		Fill(x - 1, y); // влево
		Fill(x, y + 1); // вниз
		y--;			// верх

		continue;
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
