/*
Составить процедуру, в результате обращения к которой из
первой заданной строки удаляется каждый символ, принадлежащий и
второй заданной строке.
Решить задачу с помощью функций типа void.
Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>

// Удаляет символ новой строки
void remove_newline(char *str)
{
    str[strcspn(str, "\n")] = '\0';
}

// Удаляет из строки s1 все символы, присутствующие в строке s2
void remove_chars(char *s1, const char *s2)
{
    int i, j, k;
    int len1 = strlen(s1);
    int len2 = strlen(s2);

    for (i = 0; i < len1; i++)
    {
        // Есть ли символ s1[i] в s2
        for (j = 0; j < len2; j++)
        {
            if (s1[i] == s2[j])
            {
                // Если найден, сдвигаем оставшуюся часть строки влево
                for (k = i; k < len1; k++)
                {
                    s1[k] = s1[k + 1];
                }
                len1--; // Уменьшаем длину строки
                i--;    // Возвращаемся на текущую позицию
                break;
            }
        }
    }
}

int main()
{
    char str1[100], str2[100];

    printf("s1: ");
    fgets(str1, sizeof(str1), stdin);
    remove_newline(str1);

    printf("s2: ");
    fgets(str2, sizeof(str2), stdin);
    remove_newline(str2);

    remove_chars(str1, str2);

    printf("Result: %s\n", str1);

    return 0;
}
