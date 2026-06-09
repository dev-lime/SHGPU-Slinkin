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

typedef struct {
    char mount_point[PATH_MAX];
    char device[PATH_MAX];
} MountEntry;

typedef struct MountNode {
    char *mount;
    char *device;
    struct MountNode *next;
} MountNode;

typedef struct FSData {
    fsid_t fsid;
    long f_type;
    unsigned long block_size;
    unsigned long total_blocks;
    unsigned long free_blocks;
    unsigned long avail_blocks;
    unsigned long total_inodes;
    unsigned long free_inodes;
    MountNode *mounts;
    int found_in_scope;
} FSData;

typedef struct FSNode {
    FSData data;
    struct FSNode *next;
} FSNode;

typedef struct {
    long f_type;
    char *name;
} FSTypeEntry;

static FSNode *fs_head = NULL;
static MountEntry *mount_list = NULL;
static int mount_count = 0;
static FSTypeEntry *fstype_table = NULL;
static int fstype_count = 0;
static char start_dir[PATH_MAX];
static char start_dir_real[PATH_MAX];

static void add_fstype(long type, const char *name) {
    for (int i = 0; i < fstype_count; i++) {
        if (fstype_table[i].f_type == type)
            return;
    }
    fstype_table = realloc(fstype_table, (fstype_count + 1) * sizeof(FSTypeEntry));
    if (!fstype_table) exit(EXIT_FAILURE);
    fstype_table[fstype_count].f_type = type;
    fstype_table[fstype_count].name = strdup(name);
    if (!fstype_table[fstype_count].name) exit(EXIT_FAILURE);
    fstype_count++;
}

static const char *get_fs_type_name(long type) {
    for (int i = 0; i < fstype_count; i++) {
        if (fstype_table[i].f_type == type)
            return fstype_table[i].name;
    }
    return "unknown";
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

        for (int i = 0; i < 4; i++) {
            p = strchr(p, ' ');
            if (!p) break;
            p++;
        }
        if (!p) continue;

        char *mount_start = p;
        char *mount_end = strchr(p, ' ');
        if (!mount_end) continue;
        *mount_end = '\0';

        mount_list = realloc(mount_list, (mount_count + 1) * sizeof(MountEntry));
        if (!mount_list) exit(EXIT_FAILURE);
        MountEntry *entry = &mount_list[mount_count];
        mount_count++;

        strncpy(entry->mount_point, mount_start, PATH_MAX - 1);
        entry->mount_point[PATH_MAX - 1] = '\0';
        size_t len = strlen(entry->mount_point);
        if (len > 1 && entry->mount_point[len - 1] == '/')
            entry->mount_point[len - 1] = '\0';

        p = mount_end + 1;
        char *dash = strstr(p, " - ");
        if (!dash) { mount_count--; continue; }
        p = dash + 3;
        char *space = strchr(p, ' ');
        if (!space) { mount_count--; continue; }
        char *fstype_str = strndup(p, space - p);
        if (!fstype_str) exit(EXIT_FAILURE);
        p = space + 1;

        char *dev_end = strchr(p, ' ');
        if (dev_end) *dev_end = '\0';
        strncpy(entry->device, p, PATH_MAX - 1);
        entry->device[PATH_MAX - 1] = '\0';

        struct statfs stfs;
        if (statfs(entry->mount_point, &stfs) == 0) {
            add_fstype(stfs.f_type, fstype_str);
        }
        free(fstype_str);
    }
    fclose(f);
}

static int compare_fsid(const fsid_t *a, const fsid_t *b) {
    return memcmp(a, b, sizeof(fsid_t));
}

static FSData *get_fs(const fsid_t *fsid) {
    FSNode *node = fs_head;
    while (node) {
        if (compare_fsid(&node->data.fsid, fsid) == 0)
            return &node->data;
        node = node->next;
    }
    FSNode *new_node = malloc(sizeof(FSNode));
    if (!new_node) exit(EXIT_FAILURE);
    memset(new_node, 0, sizeof(FSNode));
    new_node->data.fsid = *fsid;
    new_node->next = fs_head;
    fs_head = new_node;
    return &new_node->data;
}

static void add_mount_to_fs(FSData *fs, const char *mount, const char *device) {
    MountNode *m = fs->mounts;
    while (m) {
        if (strcmp(m->mount, mount) == 0) return;
        m = m->next;
    }
    MountNode *new_mount = malloc(sizeof(MountNode));
    if (!new_mount) exit(EXIT_FAILURE);
    new_mount->mount = strdup(mount);
    new_mount->device = strdup(device);
    new_mount->next = fs->mounts;
    fs->mounts = new_mount;
    fs->found_in_scope = 1;
}

static void record_fs(struct statfs *stfs, const char *path) {
    FSData *fs = get_fs(&stfs->f_fsid);

    if (fs->f_type == 0) {
        fs->f_type = stfs->f_type;
        fs->block_size = stfs->f_bsize;
        fs->total_blocks = stfs->f_blocks;
        fs->free_blocks = stfs->f_bfree;
        fs->avail_blocks = stfs->f_bavail;
        fs->total_inodes = stfs->f_files;
        fs->free_inodes = stfs->f_ffree;
    }

    for (int i = 0; i < mount_count; i++) {
        if (strcmp(mount_list[i].mount_point, path) == 0) {
            add_mount_to_fs(fs, path, mount_list[i].device);
            break;
        }
    }
}

static int is_inside_start_dir(const char *path) {
    char real[PATH_MAX];
    if (realpath(path, real) == NULL) return 0;

    if (strcmp(start_dir_real, "/") == 0) return 1;

    size_t len = strlen(start_dir_real);
    if (strncmp(real, start_dir_real, len) != 0) return 0;
    return (real[len] == '\0' || real[len] == '/');
}

static void scan_directory(const char *path) {
    struct statfs stfs;
    if (statfs(path, &stfs) == 0)
        record_fs(&stfs, path);

    DIR *dir = opendir(path);
    if (!dir) {
        if (errno != EACCES && errno != ENOENT && errno != ELOOP)
            fprintf(stderr, "Warning: Cannot open '%s': %s\n", path, strerror(errno));
        return;
    }

    struct dirent *entry;
    char full_path[PATH_MAX];
    struct stat st;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        if (strcmp(path, "/") == 0)
            snprintf(full_path, sizeof(full_path), "/%s", entry->d_name);
        else
            snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);

        if (lstat(full_path, &st) != 0) continue;
        if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode)) {
            if (!is_inside_start_dir(full_path))
                fprintf(stderr, "Warning: '%s' is outside the start directory, skipping\n", full_path);
            else
                scan_directory(full_path);
        }
    }
    closedir(dir);
}

static void print_size_human(unsigned long blocks, unsigned long block_size, const char *label) {
    unsigned long long total_bytes = (unsigned long long)blocks * block_size;
    double gb = (double)total_bytes / (1024.0*1024.0*1024.0);
    double mb = (double)total_bytes / (1024.0*1024.0);
    if (gb >= 1.0)
        printf("    %-20s %lu blocks (%.2f GB)\n", label, blocks, gb);
    else if (mb >= 1.0)
        printf("    %-20s %lu blocks (%.2f MB)\n", label, blocks, mb);
    else
        printf("    %-20s %lu blocks (%llu KB)\n", label, blocks, total_bytes / 1024);
}

static void print_results() {
    if (!fs_head) {
        printf("No filesystems found or accessible.\n");
        return;
    }

    printf("Filesystem Analysis Results\n");
    printf("Search Directory: %s\n", start_dir);

    for (FSNode *fn = fs_head; fn; fn = fn->next) {
        int already = 0;
        for (FSNode *prev = fs_head; prev != fn; prev = prev->next) {
            if (prev->data.f_type == fn->data.f_type) {
                already = 1;
                break;
            }
        }
        if (already) continue;

        const char *type_name = get_fs_type_name(fn->data.f_type);

        printf("---------------------------------------------------------------\n");
        printf("Filesystem Type: %s\n", type_name);

        int instance_num = 1;
        for (FSNode *in = fs_head; in; in = in->next) {
            if (in->data.f_type != fn->data.f_type) continue;

            FSData *fs = &in->data;
            printf("\n  [Instance %d]\n", instance_num++);
            printf("    FS ID:            %d:%d\n", fs->fsid.__val[0], fs->fsid.__val[1]);
            printf("    Block Size:       %lu bytes\n", fs->block_size);
            print_size_human(fs->total_blocks, fs->block_size, "Total Space:");
            print_size_human(fs->free_blocks, fs->block_size, "Free Space:");
            print_size_human(fs->avail_blocks, fs->block_size, "Avail Space:");
            printf("    Total Inodes:     %lu\n", fs->total_inodes);
            printf("    Free Inodes:      %lu\n", fs->free_inodes);

            printf("    Mount Points (in scope):\n");
            if (fs->found_in_scope && fs->mounts) {
                MountNode *m = fs->mounts;
                while (m) {
                    printf("      - %s (device: %s)\n", m->mount, m->device);
                    m = m->next;
                }
            } else {
                printf("      None found within search directory\n");
            }
            printf("    -----------------------------------------------\n");
        }
        printf("\n");
    }
}

static void cleanup() {
    FSNode *fn = fs_head;
    while (fn) {
        FSNode *next = fn->next;
        MountNode *m = fn->data.mounts;
        while (m) {
            MountNode *mn = m->next;
            free(m->mount);
            free(m->device);
            free(m);
            m = mn;
        }
        free(fn);
        fn = next;
    }

    free(mount_list);

    for (int i = 0; i < fstype_count; i++)
        free(fstype_table[i].name);
    free(fstype_table);
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

    size_t len = strlen(start_dir_real);
    if (len > 1 && start_dir_real[len - 1] == '/')
        start_dir_real[len - 1] = '\0';

    load_mountinfo();
    scan_directory(start_dir_real);
    print_results();
    cleanup();
    return 0;
}
