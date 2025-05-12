/*
Натуральное число из n цифр является числом Армстронга,
если сумма его цифр, возведенных в n-ю степень, равна самому числу
(как, например, 153 = 1^3 + 5^3 + 3^3). Получить все числа Армстронга,
состоящие из двух, трех и четырех цифр.
*/

#include <stdio.h>
#include <math.h>

int main()
{
    int N, num, temp, sum, digit, count;
    scanf("%d", &N);

    int start = pow(10, N - 1), end = pow(10, N);
    for (num = start; num < end; num++)
    {
        temp = num;
        sum = 0;
        count = N;
        while (temp)
        {
            digit = temp % 10;
            sum += pow(digit, count);
            temp /= 10;
        }
        if (sum == num)
            printf("%d\n", num);
    }

    return 0;
}
