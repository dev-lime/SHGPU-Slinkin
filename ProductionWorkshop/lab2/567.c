#include <stdio.h>

int main()
{
    int n;
    scanf("%d", &n);
    
    long long fact = 1;
    for (int i = 2; i <= n; i++)
        fact *= i;
    
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
