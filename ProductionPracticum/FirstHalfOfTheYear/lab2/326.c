/*
Найти наименьшее натуральное число n, представимое
двумя различными способами в виде суммы кубов двух натуральных
чисел x^3 + y^3 (x ≥ y).
*/

#include <stdio.h>

int main()
{
    int edge, ln;
    scanf("%d", &edge);

    for (int n = 2; n < edge; n++)
    {
        int count = 0;
        for (int x = 1; x * x * x < n; x++)
        {
            for (int y = x; y * y * y < n; y++)
            {
                if (x * x * x + y * y * y == n)
                    count++;
                if (count > 1 & ln != n)
                {
                    ln = n;
                    printf("n: %d\t x: %d\t y:%d\n", n, x, y);
                }
            }
        }
    }
}
