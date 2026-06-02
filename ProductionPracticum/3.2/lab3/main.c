/*
Современные анализаторы журналов перед решением задач анализа обычно агрегируют информацию из различных источников в одной базе данных.
Предположим, что веб-сервер сохраняет информацию о своей посещаемости в файлах-журналах, генерируя по одному файлу в сутки.
Каждая строка журнала содержит стандартизованный набор информации, которую, перед анализом, надо агрегировать в отдельном целевом файле.
Если задача выполняется на многопроцессорной системе, программа-агрегатор имеет возможность значительно ускорить свою работу,
распараллелив обработку исходных журналов.

Задача заключается в разработке программы-агрегатора, которая в качестве параметров командной строки получает
имя целевого файла и набор имен файлов журнала. Программа распараллеливает свою работу с использованием fork,
обеспечивая обработку каждого файла журнала в отдельном процессе.
Результат обработки каждый процесс конкурентно размещает в целевом файле, учитывая его предыдущее содержимое.
Для обеспечения эксклюзивного доступа к целевому файлу в агрегаторе следует использовать любые известные файловые блокировки (flock, lockf)

Для обработки предлагается использовать набор журналов доступа к веб-порталу ШГПУ за 6 дней января 2021 года.
Для изменения файла журнала можно воспользоваться "перемешивателем" shuffle.pas.
Каждый журнал представлет собой csv-файл, первая строка которого содержит набор имен полей, например:

"id";"remote_host";"date_time";"method";"server_name";"port";"directory";"file_name";"query";"status";"recv_seconds";"recv_bytes";"user_agent"
"1";"10.0.10.10";"2021-01-01 00:00:01";"GET";"files.shgpi.edu.ru";"80";"/files/rasp/faculty/f05/11_02_2013_17_02_2013/";"11_02_2013_17_02_2013.doc";;"404";"0";"261";"Googlebot/2.1 (+http://www.google.com/bot.html)"
"2";"54.36.148.122";"2021-01-01 00:00:00";"GET";"shgpi.edu.ru";"80";"/special/struktura-universiteta/nauka/novosti-nauka/6ccc0a74381abde5d6e756edb95af498/";;"?L=556&tx_ttnews%5Bcat%5D=3&tx_ttnews%5Bpointer%5D=33";"200";"1";"100612";"Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)"
"3";"66.249.64.192";"2021-01-01 00:00:01";"GET";"shgpi.edu.ru";"80";"/files/rasp/faculty/f05/11_02_2013_17_02_2013/";"11_02_2013_17_02_2013.doc";;"404";"0";"261";"Googlebot/2.1 (+http://www.google.com/bot.html)"
"4";"109.252.201.243";"2021-01-01 00:00:00";"GET";"lib.shgpi.edu.ru";"443";"/forum/img/besedka/gorockop/";"pic.php222.gif";;"200";"0";"415747";"Mozilla/5.0 (Linux; Android 10; SM-M315F Build/QP1A.190711.020; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/81.0.4044.138 Mobile Safari/537.36 YandexSearch/8.70 YandexSearchWebView/8.70"
"5";"54.36.148.204";"2021-01-01 00:00:00";"GET";"shgpi.edu.ru";"80";"/en/studentu/novosti-studentu/voprosy-gumanitarnogo-obrazovanija-obsudili-v-khode-molodjozhnoi-konferencii/";;;"200";"0";"73195";"Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)"

Целевой файл должен содержать количество посещений для каждого уникального имени сервера веб-портала ШГПУ (повторение имен недопустимо), например:
shgpi.edu.ru 12345
lib.shgpi.edu.ru 234
vt.shgpi.edu.ru 567
files.shgpi.edu.ru 4321

Пример запуска программы:
./aggrlogs result.txt 2021_01_01_shgpi_edu_ru.csv 2021_01_02_shgpi_edu_ru.csv 2021_01_03_shgpi_edu_ru.csv 2021_01_04_shgpi_edu_ru.csv
В данном примере из текущего каталога запускается агрегатор aggrlog, результат обработки размещается в файле result.txt,
обрабатываться файлы журналов 2021_01_02_shgpi_edu_ru.csv, 2021_01_03_shgpi_edu_ru.csv и 2021_01_04_shgpi_edu_ru.csv, каждый в отдельном процессе.
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
