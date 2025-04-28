/*
Дано натуральное число n (n ≤ 9999).
Является ли это число палиндромом (перевертышем) с учетом
четырех цифр, как, например, числа 2222, 6116, 0440 и т. д.?
Решить задачу, с помощью функций и макроопределений, предназначенных для использования в выражениях.
Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define IS_PALINDROME(s) (strlen(s) == 1 ||                   \
                          (strlen(s) == 2 && s[0] == s[1]) || \
                          (strlen(s) == 3 && s[0] == s[2]) || \
                          (strlen(s) == 4 && s[0] == s[3] && s[1] == s[2]))

int is_palindrome(const char *s)
{
    int len = strlen(s);

    return (len == 1) ||
           (len == 2 && s[0] == s[1]) ||
           (len == 3 && s[0] == s[2]) ||
           (len == 4 && s[0] == s[3] && s[1] == s[2]);
}

int main()
{
    char n[5];
    printf("n: ");
    scanf("%4s", n);

    if (IS_PALINDROME(n))
    {
        printf("M: %s is palindrome\n", n);
    }
    else
    {
        printf("M: %s is not palindrome\n", n);
    }

    if (is_palindrome(n))
    {
        printf("F: %s is palindrome\n", n);
    }
    else
    {
        printf("F: %s is not palindrome\n", n);
    }

    return 0;
}
