#include <stdio.h>
#include <math.h>

int main()
{
    int M, N, maxSum = 0, result = 0;
    scanf("%d %d", &M, &N);

    for (int i = M; i <= N; i++)
    {
        int sum = 0;
        for (int j = 1; j <= (int)sqrt(i); j++)
        {
            if (i % j == 0)
            {
                sum += j;
                if (j != i / j)
                    sum += i / j;
            }
        }
        if (sum > maxSum)
        {
            maxSum = sum;
            result = i;
        }
    }

    printf("%d\n", result);
    return 0;
}
