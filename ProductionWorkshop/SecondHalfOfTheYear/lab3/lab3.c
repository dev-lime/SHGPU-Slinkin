/*

Создать утилиту cml (упрощенный аналог утилит cp, mv, ln) для копирования/перемещения регулярных файлов и создания ссылок на регулярные файлы. Общий вид:
 cml [-h|-l|-m] исходный_файл результирующий_файл
Поведение утилиты:
1) Если синтаксис вызова утилиты неверен (отсутствуют один или оба обязательных параметра, обязательных параметров больше двух, используются несуществующие опции) - сообщить об этом, вывести краткую справку и остановить программу.
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
5) Если результирующий файл не существует, то утилита либо копирует исходный файл (если опция отсутствует), либо перемещает/переименовывает исходный файл (-m), либо создает жесткую ссылку (-h), либо создает символическую ссылку (-l)
6) Если результирующий файл существует и является регулярным файлом или ссылкой, следует запросить у пользователя разрешение на удаление. При положительном ответе - удалить файл и далее работать по предыдущему варианту. При отрицательном - остановить программу с соответствующим сообщением.

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
    printf("Usage: cml [-h|-l|-m] source_file target_file\n");
    printf("Options:\n");
    printf("  -h  Create a hard link\n");
    printf("  -l  Create a symbolic link\n");
    printf("  -m  Move/rename the file\n");
    printf("If no option is specified, the file will be copied.\n");
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
        perror("Error opening source file");
        return 0;
    }

    dst_fd = open(dst, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (dst_fd == -1)
    {
        perror("Error opening destination file");
        close(src_fd);
        return 0;
    }

    while ((bytes_read = read(src_fd, buffer, BUFFER_SIZE)) > 0)
    {
        bytes_written = write(dst_fd, buffer, bytes_read);
        if (bytes_written != bytes_read)
        {
            perror("Error writing to destination file");
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
    int opt;
    int h_flag = 0, l_flag = 0, m_flag = 0;
    char *source_file = NULL;
    char *target_file = NULL;

    // Parse command line options
    while (opt = getopt(argc, argv, "hlm")) {
        if (opt == -1)
            break;

        switch (opt)
        {
        case 'h':
            h_flag = 1;
            break;
        case 'l':
            l_flag = 1;
            break;
        case 'm':
            m_flag = 1;
            break;
        default:
            fprintf(stderr, "Error: Unknown option '-%c'\n", optopt);
            print_help();
            return 1;
        }
    }

    // Check for mutually exclusive options
    if ((h_flag + l_flag + m_flag) > 1) {
        fprintf(stderr, "Error: Options -h, -l, -m are mutually exclusive\n");
        print_help();
        return 1;
    }

    // Check for correct number of non-option arguments
    if (optind + 2 != argc) {
        fprintf(stderr, "Error: Incorrect number of arguments\n");
        print_help();
        return 1;
    }

    source_file = argv[optind];
    target_file = argv[optind + 1];

    // Check if source file exists and is regular file or link
    if (!is_regular_file(source_file)) {
        fprintf(stderr, "Error: Source file '%s' doesn't exist or is not a regular file/link\n", source_file);
        return 1;
    }

    // Check if target file exists
    struct stat target_stat;
    int target_exists = (stat(target_file, &target_stat) == 0);

    if (target_exists) {
        // Check if target is regular file or link
        if (!is_regular_file(target_file))
        {
            fprintf(stderr, "Error: Target file '%s' exists but is not a regular file/link\n", target_file);
            return 1;
        }

        // Ask for user confirmation
        printf("Target file '%s' exists. Overwrite? (y/n): ", target_file);
        char response;
        scanf(" %c", &response);
        if (response != 'y' && response != 'Y')
        {
            printf("Operation canceled\n");
            return 0;
        }

        // Remove existing file
        if (unlink(target_file) != 0)
        {
            perror("Error removing target file");
            return 1;
        }
    }

    // Perform the requested operation
    if (h_flag) {
        if (link(source_file, target_file) != 0)
        {
            perror("Error creating hard link");
            return 1;
        }
        printf("Hard link created from '%s' to '%s'\n", source_file, target_file);
    } else if (l_flag) {
        if (symlink(source_file, target_file) != 0)
        {
            perror("Error creating symbolic link");
            return 1;
        }
        printf("Symbolic link created from '%s' to '%s'\n", source_file, target_file);
    } else if (m_flag) {
        if (rename(source_file, target_file) != 0)
        {
            perror("Error moving file");
            return 1;
        }
        printf("File moved from '%s' to '%s'\n", source_file, target_file);
    } else {
        if (!copy_file(source_file, target_file))
        {
            return 1;
        }
        printf("File copied from '%s' to '%s'\n", source_file, target_file);
    }

    return 0;
}
