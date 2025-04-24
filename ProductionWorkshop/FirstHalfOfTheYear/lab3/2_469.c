/*
Выравнивание строки заключается в том, что между ее
отдельными словами  (см. задачу 269)  дополнительно вносятся
пробелы так, чтобы длина строки стала равной заданной длине
(предполагается, что требуемая длина не меньше исходной), а
последнее слово строки сдвинулось к ее правому краю. Составить
процедуру выравнивания заданной строки текста.
Решить задачу с помощью функций типа void.

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

void align_right(char *str, int target_len)
{
    int current_len = strlen(str);
    if (target_len <= current_len)
        return;

    // Счётчик слов
    int word_count = 0;
    int in_word = 0;
    for (int i = 0; str[i]; i++)
    {
        if (str[i] != ' ' && !in_word)
        {
            in_word = 1;
            word_count++;
        }
        else if (str[i] == ' ')
        {
            in_word = 0;
        }
    }

    if (word_count <= 1)
        return;

    // Счётчик пробелов
    int total_spaces = target_len - current_len;
    int spaces_per_gap = total_spaces / (word_count - 1);
    int extra_spaces = total_spaces % (word_count - 1);

    // Новая строка
    char *result = (char *)malloc(target_len + 1);
    int pos = 0;
    in_word = 0;
    int gap_num = 0;

    for (int i = 0; str[i]; i++)
    {
        if (str[i] != ' ')
        {
            result[pos++] = str[i];
            in_word = 1;
        }
        else if (in_word)
        {
            in_word = 0;
            gap_num++;
            // Добавляет пробелы
            int spaces = spaces_per_gap + (gap_num <= extra_spaces ? 1 : 0);
            for (int j = 0; j < spaces; j++)
            {
                result[pos++] = ' ';
            }
        }
    }
    result[pos] = '\0';

    // Копирует результат обратно в исходную строку
    strcpy(str, result);
    free(result);
}

int main()
{
    char str[1000];
    int target_len;

    printf("Введите строку: ");
    fgets(str, sizeof(str), stdin);
    str[strcspn(str, "\n")] = '\0';

    printf("Введите целевую длину строки: ");
    scanf("%d", &target_len);

    align_right(str, target_len);

    printf("Выровненная строка:\n%s\n", str);
    printf("Длина строки: %zu\n", strlen(str));

    return 0;
}
