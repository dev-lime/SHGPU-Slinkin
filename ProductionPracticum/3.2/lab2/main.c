/*
Разработать программу, которая перебирает все каталоги, начиная от заданного и ниже.
По окончании перебора выводит полную информацию о всех найденных уникальных файловых
системах в человекочитаемом формате, сгруппированных по типу файловой системы.
Для решения задачи рекомендуется использовать функции модуля dirent.h,
системную функцию statfs или ее posix-аналог statvfs.
Уникальность файловой системы определяется по ее идентификатору
(например struct statfs.f_fsid).Выход за пределы заданного каталога не допускается.

Вывод должен включать точки монтирования всех найденных файловых систем,
если они находятся в пределах заданного каталога и ниже.
Если ни одна точка монтирования конкретной файловой системы
не находится в пределах каталога, то следует сообщить о данном факте.

Для определения текущих точек монтирования файловых систем рекомендуется использовать
текстовый файл /etc/mtab, где второе поле в каждой строке содержит абсолютный путь
к примонтированной файловой системе, либо текстовый файл /proc/self/mountinfo,
где пятое поле каждой строки содержит ту-же информацию.

Альтернативный способ определения текущих точек монтирования файловых систем:
получение дескриптора монтирования с помощью системной функции statx и
обработка текстового файла /proc/self/mountinfo, где первое поле каждой строки -
дескриптор монтирования, а пятое поле - абсолютный путь к примонтированной файловой системе.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/statfs.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <ctype.h>

#define MAX_FS_ENTRIES 128
#define MAX_MOUNTS 512
#define MAX_MOUNTS_PER_FS 32
#define MAX_DEVICES 512

// Структура для хранения точки монтирования из /proc/self/mountinfo
struct MountEntry {
    char mount_point[PATH_MAX];
    char device[PATH_MAX];
    char fs_type[64];
};

// Структура для хранения уникальной ФС
struct FSData {
    fsid_t fsid;
    long f_type;
    char type_name[64];
    unsigned long block_size;
    unsigned long total_blocks;
    unsigned long free_blocks;
    unsigned long avail_blocks;
    unsigned long total_inodes;
    unsigned long free_inodes;
    char devices[MAX_MOUNTS_PER_FS][PATH_MAX];
    char mount_points[MAX_MOUNTS_PER_FS][PATH_MAX];
    int info_count;
    int found_in_scope; // Флаг найдена ли точка монтирования в пределах каталога
};

static struct FSData found_fs[MAX_FS_ENTRIES];
static int fs_count = 0;

static struct MountEntry mount_list[MAX_MOUNTS];
static int mount_count = 0;

static char start_dir[PATH_MAX];
static char start_dir_real[PATH_MAX];

// Определение имени типа ФС по f_type
const char *get_fs_type_name(long type) {
    switch (type) {
        case 0xef53:      return "ext2/ext3/ext4";
        case 0x01021994:  return "tmpfs";
        case 0x58465342:  return "xfs";
        case 0x9123683e:  return "btrfs";
        case 0x6969:      return "nfs";
        case 0x9fa0:      return "proc";
        case 0x62656572:  return "sysfs";
        case 0x64626720:  return "debugfs";
        case 0x00c0ffee:  return "cgroup";
        case 0x63677270:  return "cgroup2";
        case 0x73717368:  return "squashfs";
        case 0x5346544e:  return "ntfs";
        case 0x4d44:      return "msdos";
        case 0x4249:      return "iso9660";
        case 0xf2f52010:  return "f2fs";
        case 0x657355cf:  return "erofs";
        case 0x61756673:  return "aufs";
        case 0x794c7630:  return "overlayfs";
        case 0x66756369:  return "cifs/smb";
        case 0x74726163:  return "tracefs";
        case 0xcafebabe:  return "befs";
        case 0x00001234:  return "hfs";
        default:          return "unknown";
    }
}

// Парсинг /proc/self/mountinfo
static void load_mountinfo() {
    FILE *f = fopen("/proc/self/mountinfo", "r");
    if (!f) {
        fprintf(stderr, "Warning: Cannot open /proc/self/mountinfo: %s\n", strerror(errno));
        return;
    }

    char line[2048];
    while (fgets(line, sizeof(line), f) && mount_count < MAX_MOUNTS) {
        char *p = line;

        /* Пропускаем поля до mount_point (5-е поле, индекс 4, после 4 пробелов)
           Формат: id parent major:minor root mount_point options ... - fstype source options
           Пропускаем 4 пробела */
        int skipped = 0;
        while (*p && skipped < 4) {
            if (*p == ' ') skipped++;
            p++;
        }
        if (!*p) continue;
        
        // p указывает на mount_point
        char *mount_start = p;
        char *mount_end = strchr(p, ' ');
        if (!mount_end) continue;
        *mount_end = '\0';
        
        strncpy(mount_list[mount_count].mount_point, mount_start, PATH_MAX - 1);
        mount_list[mount_count].mount_point[PATH_MAX - 1] = '\0';
        // Нормализация: убираем trailing slash
        size_t len = strlen(mount_list[mount_count].mount_point);
        if (len > 1 && mount_list[mount_count].mount_point[len - 1] == '/') {
            mount_list[mount_count].mount_point[len - 1] = '\0';
        }

        p = mount_end + 1;

        // разделитель ' - '
        char *dash = strstr(p, " - ");
        if (!dash) continue;
        dash += 3; /* После ' - ' */
        
        // dash указывает на fstype
        char *fstype_start = dash;
        char *fstype_end = strchr(fstype_start, ' ');
        if (fstype_end) {
            *fstype_end = '\0';
            strncpy(mount_list[mount_count].fs_type, fstype_start, sizeof(mount_list[mount_count].fs_type) - 1);
            p = fstype_end + 1;
        } else {
            strncpy(mount_list[mount_count].fs_type, fstype_start, sizeof(mount_list[mount_count].fs_type) - 1);
            continue;
        }

        // p теперь указывает на source (device)
        char *source_start = p;
        char *source_end = strchr(source_start, ' ');
        if (source_end) {
            *source_end = '\0';
            strncpy(mount_list[mount_count].device, source_start, PATH_MAX - 1);
        } else {
            strncpy(mount_list[mount_count].device, source_start, PATH_MAX - 1);
        }
        mount_list[mount_count].device[PATH_MAX - 1] = '\0';

        mount_count++;
    }
    fclose(f);
}

// Сравнение fsid_t
static int compare_fsid(const fsid_t *a, const fsid_t *b) {
    return memcmp(a, b, sizeof(fsid_t));
}

// Поиск индекса ФС по fsid
static int find_fs_index(const fsid_t *target_fsid) {
    int i;
    for (i = 0; i < fs_count; i++) {
        if (compare_fsid(&found_fs[i].fsid, target_fsid) == 0) {
            return i;
        }
    }
    return -1;
}

// Запись/обновление информации о ФС
static void record_fs(struct statfs *stfs, const char *path) {
    int idx = find_fs_index(&stfs->f_fsid);
    if (idx != -1) {
        // Обновляет информацию, если нашли новую точку монтирования или устройство
        int j;
        for (j = 0; j < mount_count; j++) {
            if (strcmp(mount_list[j].mount_point, path) == 0) {
                int k;
                int already_added = 0;
                for (k = 0; k < found_fs[idx].info_count; k++) {
                    if (strcmp(found_fs[idx].mount_points[k], path) == 0) {
                        already_added = 1;
                        break;
                    }
                }
                if (!already_added && found_fs[idx].info_count < MAX_MOUNTS_PER_FS) {
                    int pos = found_fs[idx].info_count;
                    strncpy(found_fs[idx].mount_points[pos], path, PATH_MAX - 1);
                    found_fs[idx].mount_points[pos][PATH_MAX - 1] = '\0';
                    strncpy(found_fs[idx].devices[pos], mount_list[j].device, PATH_MAX - 1);
                    found_fs[idx].devices[pos][PATH_MAX - 1] = '\0';
                    found_fs[idx].info_count++;
                }
                found_fs[idx].found_in_scope = 1;
                break;
            }
        }
        // Если путь не точка монтирования, но ФС уже известна, ничего не делаем.
        return;
    }

    // Новая ФС
    if (fs_count >= MAX_FS_ENTRIES) return;

    idx = fs_count;
    memset(&found_fs[idx], 0, sizeof(struct FSData));
    found_fs[idx].fsid = stfs->f_fsid;
    found_fs[idx].f_type = stfs->f_type;
    strncpy(found_fs[idx].type_name, get_fs_type_name(stfs->f_type), sizeof(found_fs[idx].type_name) - 1);
    found_fs[idx].block_size = stfs->f_bsize;
    found_fs[idx].total_blocks = stfs->f_blocks;
    found_fs[idx].free_blocks = stfs->f_bfree;
    found_fs[idx].avail_blocks = stfs->f_bavail;
    found_fs[idx].total_inodes = stfs->f_files;
    found_fs[idx].free_inodes = stfs->f_ffree;
    found_fs[idx].info_count = 0;
    found_fs[idx].found_in_scope = 0;

    int j;
    for (j = 0; j < mount_count; j++) {
        if (strcmp(mount_list[j].mount_point, path) == 0) {
            strncpy(found_fs[idx].mount_points[0], path, PATH_MAX - 1);
            found_fs[idx].mount_points[0][PATH_MAX - 1] = '\0';
            strncpy(found_fs[idx].devices[0], mount_list[j].device, PATH_MAX - 1);
            found_fs[idx].devices[0][PATH_MAX - 1] = '\0';
            found_fs[idx].info_count = 1;
            found_fs[idx].found_in_scope = 1;
            break;
        }
    }

    fs_count++;
}

// Проверка находится ли path внутри start_dir
static int is_inside_start_dir(const char *path) {
    char real[PATH_MAX];
    if (realpath(path, real) == NULL) return 0;

    if (strcmp(start_dir_real, "/") == 0) {
        return (real[0] == '/');
    }

    size_t start_len = strlen(start_dir_real);
    while (start_len > 1 && start_dir_real[start_len - 1] == '/') {
        start_len--;
    }

    if (strncmp(real, start_dir_real, start_len) != 0) return 0;
    if (real[start_len] == '\0' || real[start_len] == '/') return 1;
    return 0;
}

// Рекурсивное сканирование
static void scan_directory(const char *path) {
    struct statfs stfs;
    
    // Проверка ФС для текущего каталога
    if (statfs(path, &stfs) == 0) {
        record_fs(&stfs, path);
    }

    DIR *dir = opendir(path);
    if (!dir) {
        /* Если каталог недоступен (например, нет прав или это mount point другой ФС, к которому нет доступа), 
           то предупреждение и продолжаем */
        if (errno != EACCES && errno != ENOENT && errno != ELOOP) {
            fprintf(stderr, "Warning: Cannot open '%s': %s\n", path, strerror(errno));
        }
        return;
    }

    struct dirent *entry;
    char full_path[PATH_MAX];
    struct stat st;

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (strcmp(path, "/") == 0)
            snprintf(full_path, sizeof(full_path), "/%s", entry->d_name);
        else
            snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

        if (lstat(full_path, &st) != 0) {
            continue;
        }

        if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode)) {
            if (!is_inside_start_dir(full_path)) {
                fprintf(stderr, "Warning: '%s' is outside the start directory, skipping\n", full_path);
            } else {
                scan_directory(full_path);
            }
        }
    }
    closedir(dir);
}

// Форматирование размера
static void print_size_human(unsigned long blocks, unsigned long block_size, const char *label) {
    unsigned long long total_bytes = (unsigned long long)blocks * block_size;
    double gb = (double)total_bytes / (1024.0 * 1024.0 * 1024.0);
    double mb = (double)total_bytes / (1024.0 * 1024.0);
    
    if (gb >= 1.0) {
        printf("    %-20s %lu blocks (%.2f GB)\n", label, blocks, gb);
    } else if (mb >= 1.0) {
        printf("    %-20s %lu blocks (%.2f MB)\n", label, blocks, mb);
        } else {
            printf("    %-20s %lu blocks (%llu KB)\n", label, blocks, total_bytes / 1024);
    }
}

// Вывод результатов
static void print_results() {
    int i, j, k;
    char *printed_types[MAX_FS_ENTRIES];
    int printed_count = 0;

    if (fs_count == 0) {
        printf("No filesystems found or accessible.\n");
        return;
    }

    printf("===============================================================\n");
    printf("Filesystem Analysis Results\n");
    printf("Search Directory: %s\n", start_dir);
    printf("===============================================================\n\n");

    for (i = 0; i < fs_count; i++) {
        // выводили ли уже этот тип
        int already_grouped = 0;
        for (k = 0; k < printed_count; k++) {
            if (strcmp(printed_types[k], found_fs[i].type_name) == 0) {
                already_grouped = 1;
                break;
            }
        }
        if (already_grouped) continue;

        // Выводит заголовок группы
        printf("---------------------------------------------------------------\n");
        printf("Filesystem Type: %s (magic: 0x%lx)\n", found_fs[i].type_name, found_fs[i].f_type);
        printf("---------------------------------------------------------------\n");

        // Выводит все экземпляры этого типа
        int instance_num = 1;
        for (j = 0; j < fs_count; j++) {
            if (strcmp(found_fs[j].type_name, found_fs[i].type_name) == 0) {
                printf("\n  [Instance %d]\n", instance_num++);
                printf("    FS ID:            %d:%d\n", 
                       found_fs[j].fsid.__val[0], found_fs[j].fsid.__val[1]);
                printf("    Block Size:       %lu bytes\n", found_fs[j].block_size);
                
                print_size_human(found_fs[j].total_blocks, found_fs[j].block_size, "Total Space:");
                print_size_human(found_fs[j].free_blocks, found_fs[j].block_size, "Free Space:");
                print_size_human(found_fs[j].avail_blocks, found_fs[j].block_size, "Avail Space:");
                
                printf("    Total Inodes:     %lu\n", found_fs[j].total_inodes);
                printf("    Free Inodes:      %lu\n", found_fs[j].free_inodes);

                printf("    Mount Points (in scope):\n");
                if (found_fs[j].found_in_scope && found_fs[j].info_count > 0) {
                    for (k = 0; k < found_fs[j].info_count; k++) {
                        printf("      - %s (device: %s)\n", 
                               found_fs[j].mount_points[k], 
                               found_fs[j].devices[k][0] ? found_fs[j].devices[k] : "unknown");
                    }
                } else {
                    printf("      None found within search directory\n");
                }
                printf("    -----------------------------------------------\n");
            }
        }
        printf("\n");

        printed_types[printed_count++] = found_fs[i].type_name;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <directory>\n", argv[0]);
        return 1;
    }

    struct stat st;
    if (stat(argv[1], &st) == -1) {
        fprintf(stderr, "Error: '%s' does not exist or is inaccessible: %s\n", argv[1], strerror(errno));
        return 1;
    }

    if (!S_ISDIR(st.st_mode)) {
        fprintf(stderr, "Error: '%s' is not a directory\n", argv[1]);
        return 1;
    }

    // абсолютный путь начального каталога для проверки "выхода за пределы"
    if (realpath(argv[1], start_dir_real) == NULL) {
        fprintf(stderr, "Error: Cannot resolve realpath for '%s': %s\n", argv[1], strerror(errno));
        return 1;
    }
    strncpy(start_dir, argv[1], PATH_MAX - 1);
    start_dir[PATH_MAX - 1] = '\0';

    load_mountinfo();
    scan_directory(start_dir_real);
    print_results();

    return 0;
}
