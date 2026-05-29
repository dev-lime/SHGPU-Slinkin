/*
Разработать программу, которая создает копию переданного ей каталога с любой глубиной вложенности,
с правами на файлы/каталоги, идентичными исходным. Символические ссылки и подкаталоги создаются как есть.
Вместо файлов других типов создаются пустые регулярные файлы.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>

int copy_directory(const char *src, const char *dst)
{
    struct stat src_stat;
    if (lstat(src, &src_stat) == -1)
    {
        fprintf(stderr, "Error: Cannot stat '%s': %s\n", src, strerror(errno));
        return -1;
    }

    if (S_ISDIR(src_stat.st_mode))
    {
        if (mkdir(dst, src_stat.st_mode) == -1 && errno != EEXIST)
        {
            fprintf(stderr, "Error: Cannot create directory '%s': %s\n", dst, strerror(errno));
            return -1;
        }

        chmod(dst, src_stat.st_mode);

        DIR *dir = opendir(src);
        if (dir == NULL)
        {
            fprintf(stderr, "Error: Cannot open directory '%s': %s\n", src, strerror(errno));
            return -1;
        }

        struct dirent *entry;
        while ((entry = readdir(dir)) != NULL)
        {
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;

            char src_path[PATH_MAX];
            char dst_path[PATH_MAX];
            snprintf(src_path, sizeof(src_path), "%s/%s", src, entry->d_name);
            snprintf(dst_path, sizeof(dst_path), "%s/%s", dst, entry->d_name);

            if (copy_directory(src_path, dst_path) == -1)
            {
                fprintf(stderr, "Warning: Failed to copy '%s'\n", src_path);
            }
        }

        closedir(dir);
    }
    else if (S_ISLNK(src_stat.st_mode))
    {
        char target[PATH_MAX];
        ssize_t len = readlink(src, target, sizeof(target) - 1);
        if (len == -1)
        {
            fprintf(stderr, "Error: Cannot read symlink '%s': %s\n", src, strerror(errno));
            return -1;
        }
        target[len] = '\0';

        if (symlink(target, dst) == -1)
        {
            fprintf(stderr, "Error: Cannot create symlink '%s': %s\n", dst, strerror(errno));
            return -1;
        }
    }
    else if (S_ISREG(src_stat.st_mode))
    {
        FILE *src_file = fopen(src, "rb");
        if (src_file == NULL)
        {
            fprintf(stderr, "Error: Cannot open file '%s': %s\n", src, strerror(errno));
            return -1;
        }

        FILE *dst_file = fopen(dst, "wb");
        if (dst_file == NULL)
        {
            fprintf(stderr, "Error: Cannot create file '%s': %s\n", dst, strerror(errno));
            fclose(src_file);
            return -1;
        }

        char buffer[4096];
        size_t bytes;
        while ((bytes = fread(buffer, 1, sizeof(buffer), src_file)) > 0)
        {
            fwrite(buffer, 1, bytes, dst_file);
        }

        fclose(src_file);
        fclose(dst_file);

        chmod(dst, src_stat.st_mode);
    }
    else
    {
        int fd = open(dst, O_CREAT | O_WRONLY, 0644);
        if (fd == -1)
        {
            fprintf(stderr, "Error: Cannot create empty file '%s': %s\n", dst, strerror(errno));
            return -1;
        }
        close(fd);
        chmod(dst, src_stat.st_mode);
    }

    return 0;
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        printf("Usage: %s <source_directory> <destination_directory>\n", argv[0]);
        return 1;
    }

    struct stat src_stat;
    if (stat(argv[1], &src_stat) == -1)
    {
        fprintf(stderr, "Error: Source '%s' does not exist or is inaccessible: %s\n", argv[1], strerror(errno));
        return 1;
    }

    if (!S_ISDIR(src_stat.st_mode))
    {
        fprintf(stderr, "Error: '%s' is not a directory\n", argv[1]);
        return 1;
    }

    if (copy_directory(argv[1], argv[2]) == -1)
    {
        fprintf(stderr, "Error: Failed to copy directory\n");
        return 1;
    }

    printf("Successfully copied '%s' to '%s'\n", argv[1], argv[2]);
    return 0;
}
