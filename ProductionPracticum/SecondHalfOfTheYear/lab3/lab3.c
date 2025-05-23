/*
Создать утилиту cml (упрощенный аналог утилит cp, mv, ln)для копирования/перемещения
регулярных файлов и создания ссылок на регулярные файлы. Общий вид:
cml [-h|-l|-m] исходный_файл результирующий_файл
Поведение утилиты:
1) Если синтаксис вызова утилиты неверен (отсутствуют один или оба обязательных параметра,
обязательных параметров больше двух, используются несуществующие опции) - сообщить об этом,
вывести краткую справку и остановить программу.
Примеры верного синтакиса:
cml -h 1 2
cml 123 456.txt
cml -h -l -x
cml -h -l
Примеры НЕверного синтакиса:
cml -k 123 456.txt
cml 1 2 3 4
cml
cml -l
2) Если исходный файл не существует - сообщить об этом и остановить программу.
3) Если исходный файл существует, но не является регулярным файлом или ссылкой - сообщить об этом и остановить программу.
4) Если результирующий файл существует, но не является регулярным файлом или ссылкой - сообщить об этом и остановить программу.
5) Если результирующий файл не существует, то утилита либо копирует исходный файл (если опция отсутствует),
либо перемещает/переименовывает исходный файл (-m), либо создает жесткую ссылку (-h), либо создает символическую ссылку (-l)
6) Если результирующий файл существует и является регулярным файлом или ссылкой,
следует запросить у пользователя разрешение на удаление.
При положительном ответе - удалить файл и далее работать по предыдущему варианту.
При отрицательном - остановить программу с соответствующим сообщением.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#define BUFFER_SIZE 4096

void print_help()
{
    printf("Использование: cml [-h|-l|-m] исходный_файл целевой_файл\n");
    printf("Опции:\n");
    printf("  -h  Создать жесткую ссылку\n");
    printf("  -l  Создать символическую ссылку\n");
    printf("  -m  Переместить/переименовать файл\n");
    printf("Если опция не указана, файл будет скопирован.\n");
}

int is_regular_file(const char *path)
{
    struct stat path_stat;
    if (stat(path, &path_stat) != 0)
    {
        return 0;
    }
    return S_ISREG(path_stat.st_mode) || S_ISLNK(path_stat.st_mode);
}

int copy_file(const char *src, const char *dst)
{
    int src_fd, dst_fd;
    ssize_t bytes_read, bytes_written;
    char buffer[BUFFER_SIZE];

    src_fd = open(src, O_RDONLY);
    if (src_fd == -1)
    {
        perror("Ошибка открытия исходного файла");
        return 0;
    }

    dst_fd = open(dst, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (dst_fd == -1)
    {
        perror("Ошибка открытия целевого файла");
        close(src_fd);
        return 0;
    }

    while ((bytes_read = read(src_fd, buffer, BUFFER_SIZE)) > 0)
    {
        bytes_written = write(dst_fd, buffer, bytes_read);
        if (bytes_written != bytes_read)
        {
            perror("Ошибка записи в целевой файл");
            close(src_fd);
            close(dst_fd);
            return 0;
        }
    }

    close(src_fd);
    close(dst_fd);
    return 1;
}

int main(int argc, char *argv[])
{
    int h_flag = 0, l_flag = 0, m_flag = 0;
    char *source_file = NULL;
    char *target_file = NULL;

    // Проверка количества аргументов
    if (argc < 3 || argc > 4)
    {
        fprintf(stderr, "Ошибка: Неверное количество аргументов\n");
        print_help();
        return 1;
    }

    // Разбор опции
    if (argc == 4)
    {
        if (strcmp(argv[1], "-h") == 0)
        {
            h_flag = 1;
        }
        else if (strcmp(argv[1], "-l") == 0)
        {
            l_flag = 1;
        }
        else if (strcmp(argv[1], "-m") == 0)
        {
            m_flag = 1;
        }
        else
        {
            fprintf(stderr, "Ошибка: Неизвестная опция '%s'\n", argv[1]);
            print_help();
            return 1;
        }

        source_file = argv[2];
        target_file = argv[3];
    }
    else
    {
        source_file = argv[1];
        target_file = argv[2];
    }

    // Проверка существования исходного файла и что это обычный файл или ссылка
    if (!is_regular_file(source_file))
    {
        fprintf(stderr, "Ошибка: Исходный файл '%s' не существует или не является обычным файлом/ссылкой\n", source_file);
        return 1;
    }

    // Проверка существования целевого файла
    struct stat target_stat;
    int target_exists = (stat(target_file, &target_stat) == 0);

    if (target_exists)
    {
        // Проверка что цель - обычный файл или ссылка
        if (!is_regular_file(target_file))
        {
            fprintf(stderr, "Ошибка: Целевой файл '%s' существует, но не является обычным файлом/ссылкой\n", target_file);
            return 1;
        }

        // Запрос подтверждения у пользователя
        printf("Целевой файл '%s' существует. Перезаписать? (y/n): ", target_file);
        char response;
        scanf(" %c", &response);
        if (response != 'y' && response != 'Y')
        {
            printf("Операция отменена\n");
            return 0;
        }

        // Удаление существующего файла
        if (unlink(target_file) != 0)
        {
            perror("Ошибка удаления целевого файла");
            return 1;
        }
    }

    // Выполнение запрошенной операции
    if (h_flag)
    {
        if (link(source_file, target_file) != 0)
        {
            perror("Ошибка создания жесткой ссылки");
            return 1;
        }
        printf("Жесткая ссылка создана из '%s' в '%s'\n", source_file, target_file);
    }
    else if (l_flag)
    {
        if (symlink(source_file, target_file) != 0)
        {
            perror("Ошибка создания символической ссылки");
            return 1;
        }
        printf("Символическая ссылка создана из '%s' в '%s'\n", source_file, target_file);
    }
    else if (m_flag)
    {
        if (rename(source_file, target_file) != 0)
        {
            perror("Ошибка перемещения файла");
            return 1;
        }
        printf("Файл перемещен из '%s' в '%s'\n", source_file, target_file);
    }
    else
    {
        if (!copy_file(source_file, target_file))
        {
            return 1;
        }
        printf("Файл скопирован из '%s' в '%s'\n", source_file, target_file);
    }

    return 0;
}
