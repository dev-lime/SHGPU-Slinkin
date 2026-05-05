/*
Task 1: Программа, которая по имени системного пользователя возвращает имена его "одногруппников",
то есть пользователей, являющихся членами хотя-бы одной из групп, в которую входит исходный пользователь.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>

#define MAX_GROUPS 128

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <username>\n", argv[0]);
        return 1;
    }

    struct passwd *pw = getpwnam(argv[1]);
    if (pw == NULL)
    {
        printf("User '%s' not found\n", argv[1]);
        return 1;
    }

    gid_t groups[MAX_GROUPS];
    int ngroups = MAX_GROUPS;
    int i, j;
    struct passwd *entry;
    int is_classmate;
    gid_t other_groups[MAX_GROUPS];
    int other_ngroups;
    struct group *gr;

    if (getgrouplist(argv[1], pw->pw_gid, groups, &ngroups) == -1)
    {
        printf("Error getting groups for user '%s'\n", argv[1]);
        return 1;
    }

    printf("User '%s' belongs to %d groups:\n", argv[1], ngroups);
    for (i = 0; i < ngroups; i++)
    {
        gr = getgrgid(groups[i]);
        if (gr != NULL)
            printf("  - %s (gid=%d)\n", gr->gr_name, groups[i]);
    }

    printf("\nClassmates (users sharing at least one group):\n");

    setpwent();

    while ((entry = getpwent()) != NULL)
    {
        if (strcmp(entry->pw_name, argv[1]) == 0)
            continue;

        other_ngroups = MAX_GROUPS;

        if (getgrouplist(entry->pw_name, entry->pw_gid, other_groups, &other_ngroups) == -1)
            continue;

        is_classmate = 0;
        for (i = 0; i < ngroups && !is_classmate; i++)
        {
            for (j = 0; j < other_ngroups && !is_classmate; j++)
            {
                if (groups[i] == other_groups[j])
                {
                    is_classmate = 1;
                }
            }
        }

        if (is_classmate)
        {
            gr = getgrgid(entry->pw_gid);
            printf("  - %s (primary group: %s)\n",
                   entry->pw_name,
                   gr != NULL ? gr->gr_name : "unknown");
        }
    }

    endpwent();
    return 0;
}
