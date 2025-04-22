/*
Составить процедуру, заменяющую в исходной строке
символов все единицы нулями и все нули единицами. Замена должна
выполняться, начиная с заданной позиции строки.
Решить задачу с помощью макроопределений. Полученные макроопределения должны вызываться как процедуры и не могут использоваться в выражениях.
*/

#include <stdio.h>
#include <string.h>

#define SWAP_ZEROS_AND_ONES(str, start_pos)          \
    do                                               \
    {                                                \
        int i;                                       \
        for (i = (start_pos); (str)[i] != '\0'; i++) \
        {                                            \
            if ((str)[i] == '0')                     \
            {                                        \
                (str)[i] = '1';                      \
            }                                        \
            else if ((str)[i] == '1')                \
            {                                        \
                (str)[i] = '0';                      \
            }                                        \
        }                                            \
    } while (0)

int main()
{
    char str[100];
    int start_pos;

    printf("String: ");
    fgets(str, sizeof(str), stdin);
    str[strcspn(str, "\n")] = '\0';

    printf("Position: ");
    scanf("%d", &start_pos);

    if (start_pos < 0 || start_pos >= strlen(str))
    {
        return 1;
    }

    SWAP_ZEROS_AND_ONES(str, start_pos);

    printf("Result: %s\n", str);

    return 0;
}
