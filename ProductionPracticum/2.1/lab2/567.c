/*
Дано натуральное число n. Выяснить, можно ли
представить n! в виде произведения трех последовательных целых
чисел.
*/

#include <stdio.h>
#include <math.h>

int main()
{
    int n;
    scanf("%d", &n);

    // Вычисляем n!
    long long fact = 1;
    for (int i = 2; i <= n; i++)
        fact *= i;

    // Начинаем перебор с кубического корня из n! и идем вниз
    long long start = (long long)cbrt(fact) + 1; // небольшой запас

    int found = 0;
    for (long long i = start; i >= 1; i--)
    {
        long long product = i * (i + 1) * (i + 2);
        if (product == fact)
        {
            printf("Yes: %lld * %lld * %lld\n", i, i + 1, i + 2);
            found = 1;
            break;
        }
        else if (product < fact)
        {
            // Произведение будет только уменьшаться, выходим
            break;
        }
    }

    if (!found)
        printf("No\n");

    return 0;
}
