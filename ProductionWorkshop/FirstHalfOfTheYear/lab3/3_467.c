/*
Составить процедуру, в результате обращения к которой из
первой заданной строки удаляется каждый символ, принадлежащий и
второй заданной строке.
Решить задачу с помощью макроопределений. Полученные макроопределения должны вызываться как процедуры и не могут использоваться в выражениях.

Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>

#define REMOVE_CHARS(s1, s2)      \
    {                             \
        char *p = (s1);           \
        char *q = (s1);           \
        while (*q)                \
        {                         \
            int found = 0;        \
            const char *r = (s2); \
            while (*r)            \
            {                     \
                if (*q == *r)     \
                {                 \
                    found = 1;    \
                    break;        \
                }                 \
                r++;              \
            }                     \
            if (!found)           \
            {                     \
                *p++ = *q;        \
            }                     \
            q++;                  \
        }                         \
        *p = '\0';                \
    }

int main()
{
    char str1[100], str2[100];

    printf("s1: ");
    fgets(str1, sizeof(str1), stdin);
    str1[strcspn(str1, "\n")] = '\0';

    printf("s2: ");
    fgets(str2, sizeof(str2), stdin);
    str2[strcspn(str2, "\n")] = '\0';

    REMOVE_CHARS(str1, str2);

    printf("Result: %s\n", str1);

    return 0;
}
