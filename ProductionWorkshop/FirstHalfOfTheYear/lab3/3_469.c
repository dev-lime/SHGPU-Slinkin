/*
Выравнивание строки заключается в том, что между ее
отдельными словами  (см. задачу 269)  дополнительно вносятся
пробелы так, чтобы длина строки стала равной заданной длине
(предполагается, что требуемая длина не меньше исходной), а
последнее слово строки сдвинулось к ее правому краю. Составить
процедуру выравнивания заданной строки текста.
Решить задачу с помощью макроопределений. Полученные макроопределения должны вызываться как процедуры и не могут использоваться в выражениях.

269. Даны натуральное число n, символы s1, ..., sn. Группы
символов, разделенные пробелами (одним или несколькими) и не
содержащие пробелов внутри себя, будем называть словами.
а) Подсчитать количество слов в данной последовательности.
б) Подсчитать количество букв а в последнем слове данной
последовательности.
в) Найти количество слов, начинающихся с буквы б.
г) Найти количество слов, у которых первый и последний
символы совпадают между собой.
д) Найти какое-нибудь слово, начинающееся с буквы а.
е) Преобразовать данную последовательность, заменяя каждое
вхождение слова это на слово то.
ж) Найти длину самого короткого слова.

Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ALIGN_RIGHT(input, output, width)                   \
    do                                                      \
    {                                                       \
        if (!(input) || !(output))                          \
            break;                                          \
                                                            \
        size_t len = strlen(input);                         \
                                                            \
        if ((width) <= len)                                 \
        {                                                   \
            *(output) = realloc(*(output), len + 1);        \
            if (*(output))                                  \
                strcpy(*(output), input);                   \
            break;                                          \
        }                                                   \
                                                            \
        /* Находит начало последнего слова */               \
        const char *last_word = input;                      \
        const char *p = input;                              \
        while (*p)                                          \
        {                                                   \
            if (*p != ' ')                                  \
            {                                               \
                last_word = p;                              \
                while (*p && *p != ' ')                     \
                    p++;                                    \
            }                                               \
            else                                            \
            {                                               \
                p++;                                        \
            }                                               \
        }                                                   \
                                                            \
        /* Вычисляет длину префикса */                      \
        size_t prefix_len = last_word - input;              \
                                                            \
        /* Выделяет память для новой строки */              \
        *(output) = realloc(*(output), (width) + 1);        \
        if (!*(output))                                     \
            break;                                          \
                                                            \
        /* Копирует префикс */                              \
        strncpy(*(output), input, prefix_len);              \
                                                            \
        /* Добавляет пробелы */                             \
        size_t spaces = (width) - len;                      \
        memset(*(output) + prefix_len, ' ', spaces);        \
                                                            \
        /* Копирует последнее слово */                      \
        strcpy(*(output) + prefix_len + spaces, last_word); \
    } while (0)

int main()
{
    char input[256];
    char *output = NULL;
    int width;

    printf("String: ");
    fgets(input, sizeof(input), stdin);
    input[strcspn(input, "\n")] = '\0';

    printf("Length: ");
    scanf("%d", &width);

    ALIGN_RIGHT(input, &output, width);

    printf("Original: [%s]\n", input);
    printf("Result:   [%s]\n", output ? output : "(null)");
    printf("Length:   %zu\n", output ? strlen(output) : 0);

    free(output);
    return 0;
}
