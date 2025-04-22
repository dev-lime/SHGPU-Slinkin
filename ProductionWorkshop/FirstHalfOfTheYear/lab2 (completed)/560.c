/*
Два натуральных числа называют дружественными, если
каждое из них равно сумме всех делителей другого, кроме самого
этого числа. Найти все пары дружественных чисел, лежащих в
диапазоне от 200 до 300.
*/

#include <stdio.h>
#include <math.h>

int main()
{
    int N, M;
    scanf("%d %d", &N, &M);

    for (int i = N; i <= M; i++)
    {
        int sum_i = 1;
        for (int d = 2; d <= (int)sqrt(i); d++)
        {
            if (i % d == 0)
            {
                sum_i += d;
                if (d != i / d)
                    sum_i += i / d;
            }
        }

        if (sum_i > i && sum_i <= M) // Только для первой половины пар
        {
            int sum_s = 1;
            for (int d = 2; d <= (int)sqrt(sum_i); d++)
            {
                if (sum_i % d == 0)
                {
                    sum_s += d;
                    if (d != sum_i / d)
                        sum_s += sum_i / d;
                }
            }

            if (sum_s == i)
            {
                printf("%d %d\n", i, sum_i);
            }
        }
    }

    return 0;
}
