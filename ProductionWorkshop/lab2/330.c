#include <stdio.h>

int main()
{
    int n;
    scanf("%d", &n);

    for (int i = 2; i < n; i++)
    {
        int sum = 0;
        for (int j = 1; j < i; j++)
            if (i % j == 0) sum += j;
        if (sum == i) printf("%d\n", i);
    }

    return 0;
}
