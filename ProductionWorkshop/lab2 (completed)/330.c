#include <stdio.h>
#include <math.h>

int main()
{
    int n;
    scanf("%d", &n);

    for (int i = 2; i < n; i++)
    {
        int sum = 1;

        for (int j = 2; j <= (int)sqrt(i); j++)
        {
            if (i % j == 0)
            {
                sum += j;
                int c = i / j;
                if (c != j)
                {
                    sum += c;
                }
            }
        }

        if (sum == i) printf("%d\n", i);
    }

    return 0;
}
