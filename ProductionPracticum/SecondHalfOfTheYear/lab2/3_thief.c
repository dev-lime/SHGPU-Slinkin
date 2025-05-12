/*
Охрана файла (низкоуровневая обработка файла)
Разработать две программы.

Первая программа, "Охранник", при запуске формирует бинарный файл secure.dat в текущем каталоге (размер файла передается в командной строке). Каждый байт secure.dat "Охранник" заполняет случайным значением от 1 до 255, последний байт в файле = 0. Затем, раз в N миллисекунд (передается в командной строке), "Охранник" считывает файл и сравнивает его с эталоном, хранящемся в оперативной памяти. Если файл отличается от эталона, "Охранник" выводит на экран 1) сообщение "Обнаружен похититель!", 2) позиции всех байтов с некорректным содержимым и 3) эталон, после чего удаляет файл и завершает работу.

Вторая программа, "Похититель", при запуске ожидает появления файла "secure.dat" в текущем каталоге и его полного заполнения, после чего пытается с периодичностью в X миллисекунд (передается в командной строке) считать из файла один байт со случайной позиции и заменить его в файле на значение 0. Позиции не должны повторяться. При исчезновении файла secure.dat "Похититель" должен вывести на экран 1) сообщение "Тревога, уходим!" и 2) позиции и значения всех байтов, которые удалось похитить, после чего завершить работу. При успешном чтении всех байтов из файла "Похититель" должен сформировать файл abducted.dat из похищенных байтов и завершить работу. При удачном "похищении" содержимое файла abducted.dat будет полностью соответствовать эталону, сформированному "Охранником".
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>

#define MAX_SIZE 1000000

int file_exists(const char *name)
{
    struct stat buffer;
    return stat(name, &buffer) == 0;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <interval_ms>\n", argv[0]);
        return 1;
    }

    int interval = atoi(argv[1]) * 1000;
    srand(time(NULL));

    while (!file_exists("secure.dat"))
    {
        usleep(100000); // 0.1 секунды
    }

    // определяет размер файла
    FILE *file = fopen("secure.dat", "rb");
    if (!file)
    {
        perror("fopen");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    int size = ftell(file);
    fclose(file);

    if (size <= 1 || size >= MAX_SIZE)
    {
        printf("Некорректный размер файла\n");
        return 1;
    }

    uint8_t *stolen = malloc(size);
    int *positions = malloc(size * sizeof(int));
    if (!stolen || !positions)
    {
        perror("malloc");
        return 1;
    }

    int stolenCount = 0;
    int *visited = calloc(size, sizeof(int));

    while (stolenCount < size)
    {
        if (!file_exists("secure.dat"))
        {
            printf("Тревога, уходим\n");
            for (int i = 0; i < stolenCount; ++i)
            {
                printf("Позиция %d: значение %u\n", positions[i], stolen[i]);
            }
            break;
        }

        int pos = rand() % size;
        if (visited[pos])
            continue;

        FILE *f = fopen("secure.dat", "rb+");
        if (!f)
            continue;

        fseek(f, pos, SEEK_SET);
        int value = fgetc(f);
        if (value == EOF)
        {
            fclose(f);
            continue;
        }

        fseek(f, pos, SEEK_SET);
        fputc(0, f);
        fclose(f);

        visited[pos] = 1;
        positions[stolenCount] = pos;
        stolen[stolenCount] = value;
        stolenCount++;

        usleep(interval);
    }

    if (stolenCount == size)
    {
        FILE *abducted = fopen("abducted.dat", "wb");
        if (!abducted)
        {
            perror("fopen");
            return 1;
        }

        for (int i = 0; i < size; ++i)
        {
            // восстанавливаем порядок
            for (int j = 0; j < size; ++j)
            {
                if (positions[j] == i)
                {
                    fputc(stolen[j], abducted);
                    break;
                }
            }
        }
        fclose(abducted);
        printf("Файл abducted.dat успешно создан\n");
    }

    free(stolen);
    free(positions);
    free(visited);
    return 0;
}
