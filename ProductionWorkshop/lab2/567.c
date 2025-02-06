#include <stdio.h>

long long factorial(int n)
{
    long long f = 1;
    for (int i = 2; i <= n; i++)
        f *= i;
    return f;
}

int main()
{
    int n;
    scanf("%d", &n);
    
    long long fact = factorial(n);
    for (long long i = 1; i * (i + 1) * (i + 2) <= fact; i++)
    {
        if (i * (i + 1) * (i + 2) == fact)
        {
            printf("Yes: %lld * %lld * %lld\n", i, i+1, i+2);
            return 0;
        }
    }

    printf("No\n");
    return 0;
}
