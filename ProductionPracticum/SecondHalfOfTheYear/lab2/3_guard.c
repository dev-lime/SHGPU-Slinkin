/*
Охрана файла (низкоуровневая обработка файла)
Разработать две программы.

Первая программа, "Охранник", при запуске формирует бинарный файл secure.dat в текущем каталоге (размер файла передается в командной строке). Каждый байт secure.dat "Охранник" заполняет случайным значением от 1 до 255, последний байт в файле = 0. Затем, раз в N миллисекунд (передается в командной строке), "Охранник" считывает файл и сравнивает его с эталоном, хранящемся в оперативной памяти. Если файл отличается от эталона, "Охранник" выводит на экран 1) сообщение "Обнаружен похититель!", 2) позиции всех байтов с некорректным содержимым и 3) эталон, после чего удаляет файл и завершает работу.

Вторая программа, "Похититель", при запуске ожидает появления файла "secure.dat" в текущем каталоге и его полного заполнения, после чего пытается с периодичностью в X миллисекунд (передается в командной строке) считать из файла один байт со случайной позиции и заменить его в файле на значение 0. Позиции не должны повторяться. При исчезновении файла secure.dat "Похититель" должен вывести на экран 1) сообщение "Тревога, уходим!" и 2) позиции и значения всех байтов, которые удалось похитить, после чего завершить работу. При успешном чтении всех байтов из файла "Похититель" должен сформировать файл abducted.dat из похищенных байтов и завершить работу. При удачном "похищении" содержимое файла abducted.dat будет полностью соответствовать эталону, сформированному "Охранником".
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <unistd.h> // для usleep
#include <sys/stat.h>

#define MAX_SIZE 1000000

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        printf("Usage: %s <size> <interval_ms>\n", argv[0]);
        return 1;
    }

    int size = atoi(argv[1]);
    int interval = atoi(argv[2]) * 1000;

    if (size <= 1 || size >= MAX_SIZE)
    {
        printf("Invalid size\n");
        return 1;
    }

    uint8_t *reference = malloc(size);
    if (!reference)
    {
        perror("malloc");
        return 1;
    }

    srand(time(NULL));
    FILE *file = fopen("secure.dat", "wb");
    if (!file)
    {
        perror("fopen");
        free(reference);
        return 1;
    }

    for (int i = 0; i < size - 1; ++i)
    {
        reference[i] = (uint8_t)(rand() % 255 + 1); // [1,255]
        fputc(reference[i], file);
    }
    reference[size - 1] = 0;
    fputc(0, file);
    fclose(file);

    while (1)
    {
        FILE *check = fopen("secure.dat", "rb");
        if (!check)
        {
            printf("Файл пропал\n");
            break;
        }

        uint8_t buffer[MAX_SIZE];
        size_t read = fread(buffer, 1, size, check);
        fclose(check);

        if (read != size || memcmp(buffer, reference, size) != 0)
        {
            printf("Обнаружен похититель\n");
            for (int i = 0; i < size; ++i)
            {
                if (buffer[i] != reference[i])
                {
                    printf("Позиция %d: ожидалось %u, найдено %u\n", i, reference[i], buffer[i]);
                }
            }

            printf("Эталон:\n");
            for (int i = 0; i < size; ++i)
            {
                printf("%u ", reference[i]);
            }
            printf("\n");

            remove("secure.dat");
            break;
        }

        usleep(interval);
    }

    free(reference);
    return 0;
}
