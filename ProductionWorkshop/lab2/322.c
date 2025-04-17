#include <stdio.h>
#include <math.h>

int main()
{
    int M, N, maxSum = 0, result = 0;
    scanf("%d %d", &M, &N);

    for (int i = M; i <= N; i++)
    {
        int sum = 0;
        for (int j = 1; j <= sqrt(i); j++)
        {
            float k = i % j;
            if (k == 0) sum += j + k;
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
// Неоптимизированный способ, можно сократить на квадрат
