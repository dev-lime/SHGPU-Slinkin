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
#include <errno.h>

typedef struct {
    char *name;
    int   count;
} ServerStat;

typedef struct {
    ServerStat *servers;
    int         num;
    int         cap;
} ServerList;

// Инициализация пустого списка
void sl_init(ServerList *sl) {
    sl->servers = NULL;
    sl->num = 0;
    sl->cap = 0;
}

// Освобождение памяти списка
void sl_free(ServerList *sl) {
    for (int i = 0; i < sl->num; i++)
        free(sl->servers[i].name);
    free(sl->servers);
    sl->servers = NULL;
    sl->num = sl->cap = 0;
}

// Поиск или добавление сервера
int sl_find_or_add(ServerList *sl, const char *name) {
    for (int i = 0; i < sl->num; i++)
        if (strcmp(sl->servers[i].name, name) == 0)
            return i;

    if (sl->num == sl->cap) {
        int newcap = (sl->cap == 0) ? 16 : sl->cap * 2;
        ServerStat *tmp = realloc(sl->servers, newcap * sizeof(ServerStat));
        if (!tmp) return -1;
        sl->servers = tmp;
        sl->cap = newcap;
    }

    sl->servers[sl->num].name = strdup(name);
    if (!sl->servers[sl->num].name) return -1;
    sl->servers[sl->num].count = 0;
    return sl->num++;
}

// парсер CSV
void parse_csv_field(const char *line, int field_num, char *out, int out_size) {
    int i = 0, cur = 0;
    while (line[i] && cur < field_num) {
        if (line[i] == ';') cur++;
        i++;
    }
    if (line[i] == '"') i++;
    int j = 0;
    while (line[i] && line[i] != '"' && line[i] != ';' && j < out_size - 1)
        out[j++] = line[i++];
    out[j] = '\0';
}

// обработка одного лог-файла
void process_log_file(const char *filename, ServerList *stats) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Не могу открыть %s: %s\n", filename, strerror(errno));
        return;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t nread;

    if ((nread = getline(&line, &len, fp)) == -1) {
        fclose(fp);
        free(line);
        return;
    }

    while ((nread = getline(&line, &len, fp)) != -1) {
        if (nread < 2) continue;
        char srv[256];
        parse_csv_field(line, 4, srv, sizeof(srv));
        if (srv[0]) {
            int idx = sl_find_or_add(stats, srv);
            if (idx >= 0) stats->servers[idx].count++;
        }
    }
    free(line);
    fclose(fp);
}

void merge_stats_to_target(const char *target, ServerList *local) {
    FILE *fp = fopen(target, "r+");
    if (!fp) fp = fopen(target, "w+");
    if (!fp) {
        fprintf(stderr, "Не могу открыть целевой файл: %s\n", target);
        return;
    }

    int fd = fileno(fp);
    flock(fd, LOCK_EX);

    ServerList global;
    sl_init(&global);

    char *line = NULL;
    size_t len = 0;

    while (getline(&line, &len, fp) != -1) {
        char srv[256];
        int cnt;
        if (sscanf(line, "%255s %d", srv, &cnt) == 2) {
            int idx = sl_find_or_add(&global, srv);
            if (idx >= 0) global.servers[idx].count = cnt;
        }
    }
    free(line);

    for (int i = 0; i < local->num; i++) {
        int idx = sl_find_or_add(&global, local->servers[i].name);
        if (idx >= 0) global.servers[idx].count += local->servers[i].count;
    }

    fseek(fp, 0, SEEK_SET);
    ftruncate(fd, 0);
    for (int i = 0; i < global.num; i++) {
        if (global.servers[i].count > 0)
            fprintf(fp, "%s %d\n", global.servers[i].name, global.servers[i].count);
    }

    flock(fd, LOCK_UN);
    fclose(fp);

    sl_free(&global);
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Использование: %s целевой_файл журнал1 [журнал2...]\n", argv[0]);
        return 1;
    }
    const char *target = argv[1];
    int nlogs = argc - 2;

    for (int i = 0; i < nlogs; i++) {
        pid_t pid = fork();
        if (pid == -1) {
            perror("fork");
            return 1;
        } else if (pid == 0) {
            ServerList local;
            sl_init(&local);
            process_log_file(argv[2 + i], &local);
            merge_stats_to_target(target, &local);
            printf("Процесс %d обработал %s, серверов: %d\n",
                   getpid(), argv[2 + i], local.num);
            sl_free(&local);
            exit(0);
        }
    }

    for (int i = 0; i < nlogs; i++) wait(NULL);
    printf("Агрегация завершена. Результат в %s\n", target);
    return 0;
}
