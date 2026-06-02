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

/* Структура для хранения точки монтирования из /proc/self/mountinfo */
typedef struct {
    char mount_point[PATH_MAX];   // путь монтирования
    char device[PATH_MAX];        // устройство
    char *fs_type;                // тип ФС (динамическая строка)
} MountEntry;

/* Структура для хранения уникальной файловой системы */
typedef struct {
    fsid_t fsid;
    long f_type;
    char *type_name;              // имя типа ФС (динамическая строка)
    unsigned long block_size;
    unsigned long total_blocks;
    unsigned long free_blocks;
    unsigned long avail_blocks;
    unsigned long total_inodes;
    unsigned long free_inodes;
    char **mount_points;          // динамический массив строк (точек монтирования)
    char **devices;               // динамический массив строк (устройств)
    int info_count;               // текущее количество записей
    int info_capacity;            // выделенная ёмкость
    int found_in_scope;           // флаг: есть точка в заданном каталоге
} FSData;

static FSData *found_fs = NULL;      // массив уникальных ФС
static int fs_count = 0;             // текущее количество ФС
static int fs_capacity = 0;          // выделенная ёмкость массива found_fs

static MountEntry *mount_list = NULL;// массив точек монтирования
static int mount_count = 0;          // количество записей
static int mount_capacity = 0;       // ёмкость массива mount_list

static char start_dir[PATH_MAX];     // исходный параметр командной строки
static char start_dir_real[PATH_MAX];// абсолютный путь (realpath)

static void add_mount_point_to_fs(FSData *fs, const char *mount, const char *dev) {
    if (fs->info_count >= fs->info_capacity) {
        fs->info_capacity = (fs->info_capacity == 0) ? 4 : fs->info_capacity * 2;
        fs->mount_points = realloc(fs->mount_points, fs->info_capacity * sizeof(char*));
        fs->devices = realloc(fs->devices, fs->info_capacity * sizeof(char*));
        if (!fs->mount_points || !fs->devices) {
            fprintf(stderr, "Memory allocation error\n");
            exit(EXIT_FAILURE);
        }
    }
    fs->mount_points[fs->info_count] = strdup(mount);
    fs->devices[fs->info_count] = strdup(dev);
    if (!fs->mount_points[fs->info_count] || !fs->devices[fs->info_count]) {
        fprintf(stderr, "Memory allocation error\n");
        exit(EXIT_FAILURE);
    }
    fs->info_count++;
    fs->found_in_scope = 1;
}

static void free_fs_data(FSData *fs) {
    free(fs->type_name);
    for (int i = 0; i < fs->info_count; i++) {
        free(fs->mount_points[i]);
        free(fs->devices[i]);
    }
    free(fs->mount_points);
    free(fs->devices);
}

static FSData* add_new_fs(void) {
    if (fs_count >= fs_capacity) {
        fs_capacity = (fs_capacity == 0) ? 16 : fs_capacity * 2;
        found_fs = realloc(found_fs, fs_capacity * sizeof(FSData));
        if (!found_fs) {
            fprintf(stderr, "Memory allocation error\n");
            exit(EXIT_FAILURE);
        }
    }
    FSData *new_fs = &found_fs[fs_count];
    memset(new_fs, 0, sizeof(FSData));
    fs_count++;
    return new_fs;
}

static MountEntry* add_mount_entry(void) {
    if (mount_count >= mount_capacity) {
        mount_capacity = (mount_capacity == 0) ? 64 : mount_capacity * 2;
        mount_list = realloc(mount_list, mount_capacity * sizeof(MountEntry));
        if (!mount_list) {
            fprintf(stderr, "Memory allocation error\n");
            exit(EXIT_FAILURE);
        }
    }
    MountEntry *entry = &mount_list[mount_count];
    memset(entry, 0, sizeof(MountEntry));
    mount_count++;
    return entry;
}

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

static void load_mountinfo() {
    FILE *f = fopen("/proc/self/mountinfo", "r");
    if (!f) {
        fprintf(stderr, "Warning: Cannot open /proc/self/mountinfo: %s\n", strerror(errno));
        return;
    }

    char line[2048];
    while (fgets(line, sizeof(line), f)) {
        char *p = line;

        /* Пропускает первые 4 пробела, чтобы встать на 5-е поле (mount_point) */
        int skipped = 0;
        while (*p && skipped < 4) {
            if (*p == ' ') skipped++;
            p++;
        }
        if (!*p) continue;

        char *mount_start = p;
        char *mount_end = strchr(p, ' ');
        if (!mount_end) continue;
        *mount_end = '\0';

        MountEntry *entry = add_mount_entry();

        strncpy(entry->mount_point, mount_start, PATH_MAX - 1);
        entry->mount_point[PATH_MAX - 1] = '\0';
        size_t len = strlen(entry->mount_point);
        if (len > 1 && entry->mount_point[len - 1] == '/') {
            entry->mount_point[len - 1] = '\0';
        }

        p = mount_end + 1;

        char *dash = strstr(p, " - ");
        if (!dash) {
            mount_count--;
            continue;
        }
        dash += 3;

        char *fstype_start = dash;
        char *fstype_end = strchr(fstype_start, ' ');
        if (fstype_end) {
            *fstype_end = '\0';
            entry->fs_type = strdup(fstype_start);
            p = fstype_end + 1;
        } else {
            entry->fs_type = strdup(fstype_start);
            mount_count--;
            continue;
        }
        if (!entry->fs_type) {
            fprintf(stderr, "Memory allocation error\n");
            exit(EXIT_FAILURE);
        }

        char *source_start = p;
        char *source_end = strchr(source_start, ' ');
        if (source_end) *source_end = '\0';
        strncpy(entry->device, source_start, PATH_MAX - 1);
        entry->device[PATH_MAX - 1] = '\0';
    }
    fclose(f);
}

static int compare_fsid(const fsid_t *a, const fsid_t *b) {
    return memcmp(a, b, sizeof(fsid_t));
}

static int find_fs_index(const fsid_t *target_fsid) {
    for (int i = 0; i < fs_count; i++) {
        if (compare_fsid(&found_fs[i].fsid, target_fsid) == 0) {
            return i;
        }
    }
    return -1;
}

static void record_fs(struct statfs *stfs, const char *path) {
    int idx = find_fs_index(&stfs->f_fsid);
    if (idx != -1) {
        for (int j = 0; j < mount_count; j++) {
            if (strcmp(mount_list[j].mount_point, path) == 0) {
                int already = 0;
                for (int k = 0; k < found_fs[idx].info_count; k++) {
                    if (strcmp(found_fs[idx].mount_points[k], path) == 0) {
                        already = 1;
                        break;
                    }
                }
                if (!already) {
                    add_mount_point_to_fs(&found_fs[idx], path, mount_list[j].device);
                }
                break;
            }
        }
        return;
    }

    FSData *new_fs = add_new_fs();
    new_fs->fsid = stfs->f_fsid;
    new_fs->f_type = stfs->f_type;
    new_fs->type_name = strdup(get_fs_type_name(stfs->f_type));
    if (!new_fs->type_name) {
        fprintf(stderr, "Memory allocation error\n");
        exit(EXIT_FAILURE);
    }
    new_fs->block_size = stfs->f_bsize;
    new_fs->total_blocks = stfs->f_blocks;
    new_fs->free_blocks = stfs->f_bfree;
    new_fs->avail_blocks = stfs->f_bavail;
    new_fs->total_inodes = stfs->f_files;
    new_fs->free_inodes = stfs->f_ffree;
    new_fs->info_count = 0;
    new_fs->info_capacity = 0;
    new_fs->mount_points = NULL;
    new_fs->devices = NULL;
    new_fs->found_in_scope = 0;

    for (int j = 0; j < mount_count; j++) {
        if (strcmp(mount_list[j].mount_point, path) == 0) {
            add_mount_point_to_fs(new_fs, path, mount_list[j].device);
            break;
        }
    }
}

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

static void scan_directory(const char *path) {
    struct statfs stfs;
    if (statfs(path, &stfs) == 0) {
        record_fs(&stfs, path);
    }

    DIR *dir = opendir(path);
    if (!dir) {
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

        if (lstat(full_path, &st) != 0) continue;

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

static void print_size_human(unsigned long blocks, unsigned long block_size, const char *label) {
    unsigned long long total_bytes = (unsigned long long)blocks * block_size;
    double gb = (double)total_bytes / (1024.0 * 1024.0 * 1024.0);
    double mb = (double)total_bytes / (1024.0 * 1024.0);
    
    if (gb >= 1.0)
        printf("    %-20s %lu blocks (%.2f GB)\n", label, blocks, gb);
    else if (mb >= 1.0)
        printf("    %-20s %lu blocks (%.2f MB)\n", label, blocks, mb);
    else
        printf("    %-20s %lu blocks (%llu KB)\n", label, blocks, total_bytes / 1024);
}

static void print_results() {
    if (fs_count == 0) {
        printf("No filesystems found or accessible.\n");
        return;
    }

    char **printed_types = NULL;
    int printed_count = 0, printed_capacity = 0;

    printf("===============================================================\n");
    printf("Filesystem Analysis Results\n");
    printf("Search Directory: %s\n", start_dir);
    printf("===============================================================\n\n");

    for (int i = 0; i < fs_count; i++) {
        int already = 0;
        for (int k = 0; k < printed_count; k++) {
            if (strcmp(printed_types[k], found_fs[i].type_name) == 0) {
                already = 1;
                break;
            }
        }
        if (already) continue;

        if (printed_count >= printed_capacity) {
            printed_capacity = (printed_capacity == 0) ? 8 : printed_capacity * 2;
            printed_types = realloc(printed_types, printed_capacity * sizeof(char*));
            if (!printed_types) {
                fprintf(stderr, "Memory allocation error\n");
                exit(EXIT_FAILURE);
            }
        }
        printed_types[printed_count] = strdup(found_fs[i].type_name);
        if (!printed_types[printed_count]) {
            fprintf(stderr, "Memory allocation error\n");
            exit(EXIT_FAILURE);
        }
        printed_count++;

        printf("---------------------------------------------------------------\n");
        printf("Filesystem Type: %s (magic: 0x%lx)\n", found_fs[i].type_name, found_fs[i].f_type);
        printf("---------------------------------------------------------------\n");

        int instance_num = 1;
        for (int j = 0; j < fs_count; j++) {
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
                    for (int k = 0; k < found_fs[j].info_count; k++) {
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
    }

    for (int i = 0; i < printed_count; i++) free(printed_types[i]);
    free(printed_types);
}

static void cleanup() {
    for (int i = 0; i < fs_count; i++) free_fs_data(&found_fs[i]);
    free(found_fs);

    for (int i = 0; i < mount_count; i++) free(mount_list[i].fs_type);
    free(mount_list);
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

    if (realpath(argv[1], start_dir_real) == NULL) {
        fprintf(stderr, "Error: Cannot resolve realpath for '%s': %s\n", argv[1], strerror(errno));
        return 1;
    }
    strncpy(start_dir, argv[1], PATH_MAX - 1);
    start_dir[PATH_MAX - 1] = '\0';

    load_mountinfo();
    scan_directory(start_dir_real);
    print_results();

    cleanup();
    return 0;
}
