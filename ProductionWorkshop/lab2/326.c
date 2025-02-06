#include <stdio.h>

int main()
{
    int n = 1;
    while (1)
    {
        int count = 0;
        for (int x = 1; x*x*x < n; x++)
        {
            for (int y = x; y*y*y < n; y++)
            {
                if (x*x*x + y*y*y == n) count++;
                if (count > 1)
                {
                    printf("%d\n", n);
                    return 0;
                }
            }
        }
        n++;
    }
}
