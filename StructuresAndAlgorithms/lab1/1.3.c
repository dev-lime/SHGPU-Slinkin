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

void FillVertical(int x, int y);

void FillHorizontal(int x, int y)
{
    AddDepth();

    // Заполнение вправо (цикл)
    int i = x;
    while (i <= MAX && IsCellEmpty(i, y))
    {
        FillCell(i, y);
        FillVertical(i, y);
        i++;
    }

    // Заполнение влево (цикл)
    i = x - 1;
    while (i >= MIN && IsCellEmpty(i, y))
    {
        FillCell(i, y);
        FillVertical(i, y);
        i--;
    }

    SubDepth();
}

void FillVertical(int x, int y)
{
    AddDepth();

    if (IsCellEmpty(x, y + 1))
    {
        FillCell(x, y + 1);
        FillHorizontal(x, y + 1);
    }

    if (IsCellEmpty(x, y - 1))
    {
        FillCell(x, y - 1);
        FillHorizontal(x, y - 1);
    }

    SubDepth();
}

void Fill(int x, int y)
{
    if (IsCellEmpty(x, y))
    {
        FillCell(x, y);
        FillHorizontal(x, y);
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
