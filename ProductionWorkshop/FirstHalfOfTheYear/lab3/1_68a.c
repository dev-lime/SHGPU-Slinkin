/*
Дано натуральное число n (n ≤ 9999).
Является ли это число палиндромом (перевертышем) с учетом
четырех цифр, как, например, числа 2222, 6116, 0440 и т. д.?
Решить задачу, с помощью функций и макроопределений, предназначенных для использования в выражениях.
*/

#include <stdio.h>

#define IS_PALINDROME(n) ((n) >= 0 && (n) <= 9999) && (((n) < 10) || ((n) < 100 && (n) / 10 == (n) % 10) || ((n) < 1000 && (n) / 100 == (n) % 10) || ((n) / 1000 == (n) % 10 && ((n) / 100) % 10 == ((n) / 10) % 10))

int is_palindrome(int n)
{
    if (n < 0 || n > 9999)
        return 0;
    int d1 = n / 1000;
    int d2 = (n / 100) % 10;
    int d3 = (n / 10) % 10;
    int d4 = n % 10;

    return (n < 10) ||
           (n < 100 && d1 == d4) ||
           (n < 1000 && d1 == d3) ||
           (d1 == d4 && d2 == d3);
}

int main()
{
    int n;
    printf("(n ≤ 9999): ");
    scanf("%d", &n);

    if (IS_PALINDROME(n))
    {
        printf("M: %d is palindrome\n", n);
    }
    else
    {
        printf("M: %d is not palindrome\n", n);
    }

    if (is_palindrome(n))
    {
        printf("F: %d is palindrome\n", n);
    }
    else
    {
        printf("F: %d is not palindrome\n", n);
    }

    return 0;
}
