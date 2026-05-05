/*
ЛР3. Параллельный агрегатор статистики (fork).
Программа получает имя целевого файла и набор имен файлов журнала.
Каждый файл журнала обрабатывается в отдельном процессе (fork).
Результат конкурентно размещается в целевом файле с использованием файловых блокировок.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <fcntl.h>
#include <errno.h>

#define MAX_SERVERS 10000
#define MAX_SERVER_LEN 256
#define MAX_LINE 4096
#define MAX_PATH_LEN 1024

typedef struct
{
    char name[MAX_SERVER_LEN];
    int count;
} ServerStat;

typedef struct
{
    ServerStat servers[MAX_SERVERS];
    int num_servers;
} ServerList;

int find_or_add_server(ServerList *list, const char *name)
{
    int i;
    for (i = 0; i < list->num_servers; i++)
    {
        if (strcmp(list->servers[i].name, name) == 0)
        {
            return i;
        }
    }
    if (list->num_servers < MAX_SERVERS)
    {
        strncpy(list->servers[list->num_servers].name, name, MAX_SERVER_LEN - 1);
        list->servers[list->num_servers].name[MAX_SERVER_LEN - 1] = '\0';
        list->servers[list->num_servers].count = 0;
        return list->num_servers++;
    }
    return -1;
}

void parse_csv_field(const char *line, int field_num, char *out, int out_size)
{
    int i = 0;
    int current_field = 0;

    while (line[i] != '\0' && current_field < field_num)
    {
        if (line[i] == ';')
        {
            current_field++;
        }
        i++;
    }

    if (line[i] == '"')
    {
        i++;
    }

    int j = 0;
    while (line[i] != '\0' && line[i] != '"' && line[i] != ';' && j < out_size - 1)
    {
        out[j++] = line[i++];
    }
    out[j] = '\0';
}

void process_log_file(const char *log_file, ServerList *local_stats)
{
    FILE *fp = fopen(log_file, "r");
    if (fp == NULL)
    {
        fprintf(stderr, "Error: Cannot open log file '%s': %s\n", log_file, strerror(errno));
        return;
    }

    char line[MAX_LINE];
    int is_header = 1;

    while (fgets(line, sizeof(line), fp) != NULL)
    {
        if (is_header)
        {
            is_header = 0;
            continue;
        }

        if (strlen(line) < 2)
        {
            continue;
        }

        char server_name[MAX_SERVER_LEN];
        parse_csv_field(line, 4, server_name, sizeof(server_name));

        if (strlen(server_name) > 0)
        {
            int idx = find_or_add_server(local_stats, server_name);
            if (idx >= 0)
            {
                local_stats->servers[idx].count++;
            }
        }
    }

    fclose(fp);
}

void load_target_file(const char *target_file, ServerList *stats)
{
    FILE *fp = fopen(target_file, "r");
    if (fp == NULL)
    {
        return;
    }

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), fp) != NULL)
    {
        char server_name[MAX_SERVER_LEN];
        int count = 0;

        if (sscanf(line, "%s %d", server_name, &count) == 2)
        {
            int idx = find_or_add_server(stats, server_name);
            if (idx >= 0)
            {
                stats->servers[idx].count = count;
            }
        }
    }

    fclose(fp);
}

void save_target_file(const char *target_file, ServerList *stats)
{
    FILE *fp = fopen(target_file, "w");
    if (fp == NULL)
    {
        fprintf(stderr, "Error: Cannot write to target file '%s': %s\n", target_file, strerror(errno));
        return;
    }

    int i;
    for (i = 0; i < stats->num_servers; i++)
    {
        if (stats->servers[i].count > 0)
        {
            fprintf(fp, "%s %d\n", stats->servers[i].name, stats->servers[i].count);
        }
    }

    fclose(fp);
}

void merge_stats_to_target(const char *target_file, ServerList *local_stats)
{
    int fd = open(target_file, O_RDWR | O_CREAT, 0644);
    if (fd == -1)
    {
        fprintf(stderr, "Error: Cannot open target file '%s': %s\n", target_file, strerror(errno));
        return;
    }

    if (flock(fd, LOCK_EX) == -1)
    {
        fprintf(stderr, "Error: Cannot lock target file '%s': %s\n", target_file, strerror(errno));
        close(fd);
        return;
    }

    ServerList global_stats;
    memset(&global_stats, 0, sizeof(global_stats));

    char *content = malloc(100 * 1024 * 1024);
    if (content == NULL)
    {
        flock(fd, LOCK_UN);
        close(fd);
        return;
    }

    off_t file_size = lseek(fd, 0, SEEK_END);
    if (file_size > 0)
    {
        lseek(fd, 0, SEEK_SET);
        ssize_t bytes_read = read(fd, content, file_size);
        if (bytes_read > 0)
        {
            content[bytes_read] = '\0';

            FILE *tmp = fmemopen(content, bytes_read, "r");
            if (tmp != NULL)
            {
                char line[MAX_LINE];
                while (fgets(line, sizeof(line), tmp) != NULL)
                {
                    char server_name[MAX_SERVER_LEN];
                    int count = 0;
                    if (sscanf(line, "%s %d", server_name, &count) == 2)
                    {
                        int idx = find_or_add_server(&global_stats, server_name);
                        if (idx >= 0)
                        {
                            global_stats.servers[idx].count = count;
                        }
                    }
                }
                fclose(tmp);
            }
        }
    }

    int i;
    for (i = 0; i < local_stats->num_servers; i++)
    {
        int idx = find_or_add_server(&global_stats, local_stats->servers[i].name);
        if (idx >= 0)
        {
            global_stats.servers[idx].count += local_stats->servers[i].count;
        }
    }

    lseek(fd, 0, SEEK_SET);
    ftruncate(fd, 0);

    char out_buf[MAX_LINE];
    for (i = 0; i < global_stats.num_servers; i++)
    {
        if (global_stats.servers[i].count > 0)
        {
            snprintf(out_buf, sizeof(out_buf), "%s %d\n", global_stats.servers[i].name, global_stats.servers[i].count);
            write(fd, out_buf, strlen(out_buf));
        }
    }

    flock(fd, LOCK_UN);
    close(fd);
    free(content);
}

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        printf("Использование: %s <целевой_файл> <файл_журнала1> [файл_журнала2] ...\n", argv[0]);
        return 1;
    }

    const char *target_file = argv[1];
    int num_logs = argc - 2;
    int i;
    pid_t pid;

    for (i = 0; i < num_logs; i++)
    {
        pid = fork();
        if (pid == -1)
        {
            fprintf(stderr, "Error: fork() failed: %s\n", strerror(errno));
            return 1;
        }
        else if (pid == 0)
        {
            ServerList local_stats;
            memset(&local_stats, 0, sizeof(local_stats));

            process_log_file(argv[2 + i], &local_stats);
            merge_stats_to_target(target_file, &local_stats);

            printf("Процесс %d: обработан файл '%s', найдено %d уникальных серверов\n",
                   getpid(), argv[2 + i], local_stats.num_servers);

            exit(0);
        }
    }

    for (i = 0; i < num_logs; i++)
    {
        int status;
        wait(&status);
    }

    printf("Агрегация завершена. Результат в файле '%s'\n", target_file);
    return 0;
}
