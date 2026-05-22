/*
Task 2: Программа, которая рекурсивно, с использованием функций dirent.h, выводит в иерархическом
представлении содержимое каталога, имя которого ей передано. Каждый выводимый элемент должен
сопровождаться информацией о типе, хозяине, группе, правах доступа в человекочитаемом формате.
Если содержимое очередного подкаталога оказывается недоступным - сообщить об этом факте для каждого
каталога и причинах его возникновения.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>

const char *get_type_string(mode_t mode)
{
    if (S_ISDIR(mode))
        return "каталог";
    else if (S_ISREG(mode))
        return "файл";
    else if (S_ISLNK(mode))
        return "символическая ссылка";
    else if (S_ISFIFO(mode))
        return "fifo-канал";
    else if (S_ISSOCK(mode))
        return "сокет";
    else if (S_ISBLK(mode))
        return "блочное устройство";
    else if (S_ISCHR(mode))
        return "символьное устройство";
    else
        return "неизвестный тип";
}

void print_permissions_human(const char *label, mode_t mode, mode_t r_bit, mode_t w_bit, mode_t x_bit)
{
    int has_r = (mode & r_bit) != 0;
    int has_w = (mode & w_bit) != 0;
    int has_x = (mode & x_bit) != 0;

    printf("%s: ", label);
    if (!has_r && !has_w && !has_x)
    {
        printf("нет прав\n");
    }
    else
    {
        if (has_r)
            printf("чтение");
        if (has_w)
            printf("%sзапись", has_r ? ", " : "");
        if (has_x)
            printf("%sисполнение", (has_r || has_w) ? ", " : "");
        printf("\n");
    }
}

void print_special_permissions(mode_t mode)
{
    int has_any = 0;

    printf("спецправа: ");

    if (mode & S_ISUID)
    {
        printf("SUID (запуск от имени хозяина)");
        has_any = 1;
    }
    if (mode & S_ISGID)
    {
        if (has_any)
            printf(", ");
        printf("SGID (запуск от имени группы)");
        has_any = 1;
    }
    if (mode & S_ISVTX)
    {
        if (has_any)
            printf(", ");
        printf("Sticky (пожелание сохранения в ОЗУ после завершения)");
        has_any = 1;
    }

    if (!has_any)
    {
        printf("нет");
    }
    printf("\n");
}

void print_entry_info(const char *path, const char *name, struct stat *st)
{
    (void)name;
    struct passwd *pw = getpwuid(st->st_uid);
    struct group *gr = getgrgid(st->st_gid);

    char link_target[PATH_MAX] = "";
    if (S_ISLNK(st->st_mode))
    {
        ssize_t len = readlink(path, link_target, sizeof(link_target) - 1);
        if (len != -1)
        {
            link_target[len] = '\0';
        }
    }

    printf("файл: %s\n", path);
    printf("тип: %s\n", get_type_string(st->st_mode));

    if (pw != NULL)
        printf("хозяин: %s (%d)\n", pw->pw_name, st->st_uid);
    else
        printf("хозяин: unknown (%d)\n", st->st_uid);

    if (gr != NULL)
        printf("группа: %s (%d)\n", gr->gr_name, st->st_gid);
    else
        printf("группа: unknown (%d)\n", st->st_gid);

    print_permissions_human("права хозяина", st->st_mode, S_IRUSR, S_IWUSR, S_IXUSR);
    print_permissions_human("права группы", st->st_mode, S_IRGRP, S_IWGRP, S_IXGRP);
    print_permissions_human("права остальных", st->st_mode, S_IROTH, S_IWOTH, S_IXOTH);
    print_special_permissions(st->st_mode);

    if (strlen(link_target) > 0)
    {
        printf("ссылка на: %s\n", link_target);
    }

    printf("\n");
}

void list_directory(const char *path, int depth)
{
    DIR *dir = opendir(path);
    if (dir == NULL)
    {
        printf("ОШИБКА: Не удалось открыть каталог '%s': %s\n\n", path, strerror(errno));
        return;
    }

    struct dirent *entry;
    int i;

    while ((entry = readdir(dir)) != NULL)
    {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[PATH_MAX];
        snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

        struct stat st;
        int stat_result;

        if (entry->d_type == DT_LNK)
        {
            stat_result = lstat(full_path, &st);
        }
        else
        {
            stat_result = stat(full_path, &st);
        }

        if (stat_result == -1)
        {
            printf("ОШИБКА: Не удалось получить информацию о '%s': %s\n\n", full_path, strerror(errno));
            continue;
        }

        for (i = 0; i < depth; i++)
            printf("  ");

        print_entry_info(full_path, entry->d_name, &st);

        if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode))
        {
            for (i = 0; i < depth; i++)
                printf("  ");
            printf("--- Содержимое каталога '%s' ---\n", full_path);
            list_directory(full_path, depth + 1);
        }
    }

    closedir(dir);
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Использование: %s <каталог>\n", argv[0]);
        return 1;
    }

    struct stat st;
    if (stat(argv[1], &st) == -1)
    {
        printf("Ошибка: '%s' не существует или недоступен: %s\n", argv[1], strerror(errno));
        return 1;
    }

    if (!S_ISDIR(st.st_mode))
    {
        printf("Ошибка: '%s' не является каталогом\n", argv[1]);
        return 1;
    }

    printf("Содержимое каталога '%s':\n\n", argv[1]);
    list_directory(argv[1], 0);

    return 0;
}
