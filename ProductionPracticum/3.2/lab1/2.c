/*
Task 2: Программа, которая рекурсивно, с использованием функций dirent.h, выводит в иерархическом
представлении содержимое каталога, имя которого ей передано. Каждый выводимый элемент должен
сопровождаться информацией о типе, хозяине, группе, правах доступа в человекочитаемом формате.
Если содержимое очередного подкаталога оказывается недоступным - сообщить об этом факте для каждого
каталога и причинах его возникновения.
файл: dir1/dir2/f1
тип: fifo-канал
хозяин: user1 (123)
группа: group1 (321)
права хозяина: чтение, запись
права группы: нет прав
права остальных: нет прав
спецправа: SUID (запуск от имени хозяина), Sticky (пожелание сохранения в ОЗУ после завершения)
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

void print_indent(int depth)
{
    for (int i = 0; i < depth; i++) {
        printf("  │ ");
    }
}

const char *get_type_string(mode_t mode)
{
    if (S_ISDIR(mode))  return "каталог";
    if (S_ISREG(mode))  return "файл";
    if (S_ISLNK(mode))  return "символическая ссылка";
    if (S_ISFIFO(mode)) return "fifo-канал";
    if (S_ISSOCK(mode)) return "сокет";
    if (S_ISBLK(mode))  return "блочное устройство";
    if (S_ISCHR(mode))  return "символьное устройство";
    return "неизвестный тип";
}

void print_permissions_human(int depth, const char *label, mode_t mode,
                             mode_t r_bit, mode_t w_bit, mode_t x_bit,
                             int is_dir)
{
    print_indent(depth);
    printf("%s: ", label);

    const char *rights[3];
    const char *dir_desc[3] = { NULL, NULL, NULL };
    int count = 0;

    if (mode & r_bit) {
        rights[count] = "чтение";
        if (is_dir)
            dir_desc[count] = "просмотр содержимого каталога";
        count++;
    }
    if (mode & w_bit) {
        rights[count] = "запись";
        if (is_dir)
            dir_desc[count] = "создание, удаление и переименование файлов";
        count++;
    }
    if (mode & x_bit) {
        rights[count] = "исполнение";
        if (is_dir)
            dir_desc[count] = "вход в каталог";
        count++;
    }

    if (count == 0) {
        if (is_dir)
            printf("нет прав (доступ к каталогу запрещён)\n");
        else
            printf("нет прав\n");
        return;
    }

    for (int i = 0; i < count; i++) {
        printf("%s", rights[i]);
        if (is_dir && dir_desc[i])
            printf(" (%s)", dir_desc[i]);
        if (i < count - 1)
            printf(", ");
    }
    printf("\n");
}

void print_special_permissions(int depth, mode_t mode, int is_dir)
{
    print_indent(depth);
    printf("спецправа: ");

    int has_any = 0;

    if (mode & S_ISUID) {
        if (is_dir)
            printf("SUID (игнорируется для каталогов)");
        else
            printf("SUID (запуск от имени хозяина)");
        has_any = 1;
    }
    if (mode & S_ISGID) {
        if (has_any) printf(", ");
        if (is_dir)
            printf("SGID (новые файлы наследуют группу каталога)");
        else
            printf("SGID (запуск от имени группы)");
        has_any = 1;
    }
    if (mode & S_ISVTX) {
        if (has_any) printf(", ");
        if (is_dir)
            printf("Sticky (удалять файлы могут только владельцы)");
        else
            printf("Sticky (пожелание сохранения в ОЗУ после завершения)");
        has_any = 1;
    }

    if (!has_any) printf("нет");
    printf("\n");
}

void print_entry_info(const char *path, struct stat *st, int depth)
{
    int is_dir = S_ISDIR(st->st_mode);

    struct passwd *pw = getpwuid(st->st_uid);
    struct group *gr = getgrgid(st->st_gid);

    char link_target[PATH_MAX] = "";
    if (S_ISLNK(st->st_mode)) {
        ssize_t len = readlink(path, link_target, sizeof(link_target) - 1);
        if (len != -1) link_target[len] = '\0';
    }

    print_indent(depth);
    printf("имя: %s\n", path);

    print_indent(depth);
    printf("тип: %s\n", get_type_string(st->st_mode));

    print_indent(depth);
    if (pw != NULL) printf("хозяин: %s (%d)\n", pw->pw_name, st->st_uid);
    else            printf("хозяин: unknown (%d)\n", st->st_uid);

    print_indent(depth);
    if (gr != NULL) printf("группа: %s (%d)\n", gr->gr_name, st->st_gid);
    else            printf("группа: unknown (%d)\n", st->st_gid);

    print_permissions_human(depth, "права хозяина", st->st_mode, S_IRUSR, S_IWUSR, S_IXUSR, is_dir);
    print_permissions_human(depth, "права группы", st->st_mode, S_IRGRP, S_IWGRP, S_IXGRP, is_dir);
    print_permissions_human(depth, "права остальных", st->st_mode, S_IROTH, S_IWOTH, S_IXOTH, is_dir);
    print_special_permissions(depth, st->st_mode, is_dir);

    if (strlen(link_target) > 0) {
        print_indent(depth);
        printf("ссылка на: %s\n", link_target);
    }

    print_indent(depth);
    printf("\n");
}

void list_directory(const char *path, int depth)
{
    DIR *dir = opendir(path);
    if (dir == NULL) {
        print_indent(depth);
        printf("ОШИБКА ДОСТУПА: Не удалось открыть каталог '%s': %s\n\n", path, strerror(errno));
        return;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[PATH_MAX];
        snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

        struct stat st;
        if (lstat(full_path, &st) == -1) {
            print_indent(depth);
            printf("ОШИБКА: Не удалось получить информацию о '%s': %s\n\n", full_path, strerror(errno));
            continue;
        }

        print_entry_info(full_path, &st, depth);

        if (S_ISDIR(st.st_mode)) {
            list_directory(full_path, depth + 1);
        }
    }

    closedir(dir);
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("Использование: %s <каталог>\n", argv[0]);
        return 1;
    }

    struct stat st;
    if (lstat(argv[1], &st) == -1) {
        printf("Ошибка: '%s' не существует или недоступен: %s\n", argv[1], strerror(errno));
        return 1;
    }

    if (!S_ISDIR(st.st_mode)) {
        printf("Ошибка: '%s' не является каталогом\n", argv[1]);
        return 1;
    }

    printf("Информация о целевом каталоге:\n");
    print_entry_info(argv[1], &st, 0);

    printf("--- Содержимое каталога ---\n\n");
    list_directory(argv[1], 0);

    return 0;
}
