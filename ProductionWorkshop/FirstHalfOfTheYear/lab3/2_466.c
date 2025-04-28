/*
Составить процедуру, заменяющую в исходной строке
символов все единицы нулями и все нули единицами. Замена должна
выполняться, начиная с заданной позиции строки.
Решить задачу с помощью функций типа void.
Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>

void swap_zeros_and_ones(char *str, int start_pos)
{
    if (start_pos < 0 || start_pos > strlen(str))
    {
        return;
    }

    for (int i = start_pos; str[i] != '\0'; i++)
    {
        if (str[i] == '0')
        {
            str[i] = '1';
        }
        else if (str[i] == '1')
        {
            str[i] = '0';
        }
    }
}

int main()
{
    char str[100];
    int start_pos;

    printf("String: ");
    fgets(str, sizeof(str), stdin);

    printf("Position: ");
    scanf("%d", &start_pos);

    swap_zeros_and_ones(str, start_pos);

    printf("Result:\n%s", str);

    return 0;
}
