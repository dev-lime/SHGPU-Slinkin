#include <stdio.h>

int main()
{
    int N, M;
    scanf("%d %d", &N, &M);

    for (int i = N; i <= M; i++)
    {
        int sum_i = 0;
        for (int d = 1; d < i; d++)
        {
            if (i % d == 0) sum_i += d;
        }

        if (sum_i > i && sum_i <= M) // Только для первой половины пар
        {
            int sum_s = 0;
            for (int d = 1; d < sum_i; d++) {
                if (sum_i % d == 0) sum_s += d;
            }

            if (sum_s == i) {
                printf("%d %d\n", i, sum_i);
            }
        }
    }

    return 0;
}
