#include <stdio.h>
#include <stdbool.h>

#define MAX 10
#define MIN 1

int Depth = 0;
int MaxDepth = 0;

char a[MAX][MAX + 1] = {
    "1---------",
    "-11--1----",
    "---1-1--11",
    "--1---11--",
    "-1-1------",
    "-1--1--111",
    "1--11-1---",
    "----1-1---",
    "-------1--",
    "--1----1--"};

void FillCell(int x, int y)
{
    a[y - 1][x - 1] = '0';
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
    if (x < MIN || x > MAX || y < MIN || y > MAX || a[y - 1][x - 1] != '-')
        return false;
    return true;
}

void FillHorizontal(int x, int y);

void FillVertical(int x, int y)
{
    // AddDepth();
    //  Заполнение вниз
    for (int i = y + 1; i <= MAX; i++)
    {
        if (!IsCellEmpty(x, i))
            break;
        FillCell(x, i);
        if (IsCellEmpty(x + 1, i))
            FillHorizontal(x + 1, i);
        if (IsCellEmpty(x - 1, i))
            FillHorizontal(x - 1, i);
    }
    // Заполнение вверх
    for (int i = y - 1; i >= MIN; i--)
    {
        if (!IsCellEmpty(x, i))
            break;
        FillCell(x, i);
        if (IsCellEmpty(x + 1, i))
            FillHorizontal(x + 1, i);
        if (IsCellEmpty(x - 1, i))
            FillHorizontal(x - 1, i);
    }
    // SubDepth();
}

void FillHorizontal(int x, int y)
{
    AddDepth();
    // Заполнение вправо
    for (int i = x; i <= MAX; i++)
    {
        if (!IsCellEmpty(i, y))
            break;
        FillCell(i, y);
        FillVertical(i, y);
    }
    // Заполнение влево
    for (int i = x - 1; i >= MIN; i--)
    {
        if (!IsCellEmpty(i, y))
            break;
        FillCell(i, y);
        FillVertical(i, y);
    }
    SubDepth();
}

void Fill(int x, int y)
{
    if (IsCellEmpty(x, y))
    {
        FillHorizontal(x, y);
    }
}

void printArr()
{
    for (int i = 0; i < MAX; i++)
    {
        printf("%s\n", a[i]);
    }
}

int main()
{
    Fill(5, 1);
    printArr();
    printf("\nMax depth = %d", MaxDepth);
    return 0;
}
