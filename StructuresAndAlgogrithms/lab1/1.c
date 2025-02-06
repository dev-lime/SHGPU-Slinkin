#include <stdio.h>

#define MAX 10
#define MIN 1

char a[MAX][MAX + 1] = {
    "----------",
    "--1--1----",
    "---1-1--11",
    "--1---11--",
    "---1------",
    "----1--111",
    "----1-1---",
    "----1-1---",
    "-------1--",
    "--1----1--"
};

void FillCell(int x, int y)
{
    a[y - 1][x - 1] = '0';
}

void FillVertical(int x, int y)
{
    if (y < MIN || y > MAX || a[y - 1][x - 1] != '-')
        return;
    
    FillCell(x, y);

	for (int i=y; i<=MAX; i++)
		FillCell(x, i);
	for (int i=y; i>=MIN; i--)
		FillCell(y, i);
}

void FillHorizontal(int x, int y)
{
    if (x < MIN || x > MAX || a[y - 1][x - 1] != '-')
        return;
    
    //FillCell(x, y);

	for (int i=x; i<=MAX; i++)
	{
		FillVertical(i, y);
	}
	for (int i=x; i>=MIN; i--)
	{
		FillVertical(i, y);
	}
}

void Fill(int x, int y)
{
    if (x < MIN || x > MAX || y < MIN || y > MAX || a[y - 1][x - 1] != '-')
        return;

    FillHorizontal(x, y);
    /*Fill(x, y + 1);
    Fill(x, y - 1);
    Fill(x + 1, y);
    Fill(x - 1, y);*/
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
    return 0;
}
