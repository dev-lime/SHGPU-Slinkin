/*
Разработать программу, которая по имени системного пользователя возвращает имена его "одногруппников",
то есть пользователей, являющихся членами хотя-бы одной из групп, в которую входит исходный пользователь.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>

// Функция для динамического получения групп пользователя
gid_t *get_user_groups(const char *username, gid_t initial_gid, int *final_ngroups)
{
    int ngroups = 0;
    
    getgrouplist(username, initial_gid, NULL, &ngroups);
    
    if (ngroups == 0) 
    {
        return NULL;
    }

    gid_t *groups = malloc(ngroups * sizeof(gid_t));
    if (groups == NULL) 
    {
        return NULL;
    }

    if (getgrouplist(username, initial_gid, groups, &ngroups) == -1) 
    {
        free(groups);
        return NULL;
    }

    *final_ngroups = ngroups;
    return groups;
}

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

    int ngroups = 0;
    gid_t *groups = get_user_groups(argv[1], pw->pw_gid, &ngroups);

    printf("User '%s' belongs to %d groups:\n", argv[1], ngroups);
    size_t max_buffer_size = 1;
    struct group *gr;
    
    for (int i = 0; i < ngroups; i++)
    {
        gr = getgrgid(groups[i]);
        if (gr != NULL)
        {
            max_buffer_size += strlen(gr->gr_name) + 2;
            printf("  - %s (gid=%d)\n", gr->gr_name, groups[i]);
        }
    }

    printf("\nClassmates (users sharing at least one group):\n");

    setpwent();
    struct passwd *entry;

    while ((entry = getpwent()) != NULL)
    {
        if (strcmp(entry->pw_name, argv[1]) == 0)
            continue;
        
        int other_ngroups = 0;
        gid_t *other_groups = get_user_groups(entry->pw_name, entry->pw_gid, &other_ngroups);
        
        if (other_groups == NULL)
            continue;

        char *shared_groups_str = malloc(max_buffer_size);
        if (shared_groups_str == NULL)
        {
            free(other_groups);
            continue;
        }
        shared_groups_str[0] = '\0';
        int has_shared = 0;

        for (int i = 0; i < ngroups; i++)
        {
            for (int j = 0; j < other_ngroups; j++)
            {
                if (groups[i] == other_groups[j])
                {
                    gr = getgrgid(groups[i]);
                    if (gr != NULL)
                    {
                        if (has_shared)
                        {
                            strcat(shared_groups_str, ", ");
                        }
                        strcat(shared_groups_str, gr->gr_name);
                        has_shared = 1;
                    }
                }
            }
        }

        if (has_shared)
        {
            printf("  - %s (%s)\n", entry->pw_name, shared_groups_str);
        }

        free(shared_groups_str);
        free(other_groups);
    }

    endpwent();
    free(groups);
    return 0;
}
