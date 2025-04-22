/*
Подсчет строк, слов и символов. (высокоуровневая обработка файла)
Создать аналог утилиты wc (word counter). Обработать опции -c -w -l короткого вида, предусмотреть возможность отсутствия имени обрабатываемого файла в командной строке. Пробельным символом (разделителем слов) считать любой символ, код которого меньше 33.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct
{
    int lines;
    int words;
    int chars;
} Counts;

void count(FILE *file, Counts *counts)
{
    int c;
    int in_word = 0;

    counts->lines = 0;
    counts->words = 0;
    counts->chars = 0;

    while ((c = fgetc(file)) != EOF)
    {
        counts->chars++;

        if (c == '\n')
        {
            counts->lines++;
        }

        if (c < 33)
        {
            if (in_word)
            {
                counts->words++;
                in_word = 0;
            }
        }
        else
        {
            in_word = 1;
        }
    }

    if (in_word)
    {
        counts->words++;
    }
}

void print_counts(Counts *counts, int print_lines, int print_words, int print_chars)
{
    if (print_lines)
    {
        printf("%d ", counts->lines);
    }
    if (print_words)
    {
        printf("%d ", counts->words);
    }
    if (print_chars)
    {
        printf("%d ", counts->chars);
    }
    printf("\n");
}

int main(int argc, char *argv[])
{
    int print_lines = 0;
    int print_words = 0;
    int print_chars = 0;
    char *filename = NULL;

    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] == '-')
        {
            for (int j = 1; argv[i][j] != '\0'; j++)
            {
                switch (argv[i][j])
                {
                case 'l':
                    print_lines = 1;
                    break;
                case 'w':
                    print_words = 1;
                    break;
                case 'c':
                    print_chars = 1;
                    break;
                default:
                    fprintf(stderr, "Unknown option: -%c\n", argv[i][j]);
                    return 1;
                }
            }
        }
        else
        {
            if (filename == NULL)
            {
                filename = argv[i];
            }
            else
            {
                fprintf(stderr, "Usage: %s [-l] [-w] [-c] [filename]\n", argv[0]);
                return 1;
            }
        }
    }

    if (!print_lines && !print_words && !print_chars)
    {
        print_lines = print_words = print_chars = 1;
    }

    Counts counts;
    FILE *file;

    if (filename != NULL)
    {
        file = fopen(filename, "r");
        if (file == NULL)
        {
            perror("Error opening file");
            return 1;
        }
    }
    else
    {
        file = stdin;
    }

    count(file, &counts);
    print_counts(&counts, print_lines, print_words, print_chars);

    if (filename != NULL)
    {
        fclose(file);
    }

    return 0;
}
