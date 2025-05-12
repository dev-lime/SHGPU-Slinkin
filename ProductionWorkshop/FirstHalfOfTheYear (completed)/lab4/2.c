/*
Общий вид функции main позволяет получить доступ к параметрам командной строки и переменным окружения:
int main(int argc, char **argv, char **env)
Разработать утилиту, которая будет выводить на экран содержимое переменных окружения (параметр env), имена которых переданы в командной строке (параметры argc и argv):
1. При отсутствии параметров в командной строке утилита должна выводить правила своего использования.
2. Поиск переменных окружения должен быть регистронезависимым.
3. Каждая переменная выводится в двух строках: имя и значение. Затем - строка разделитель вида "---"
4. При отсутствии в окружении искомой переменной, утилита должна выводить пустую строку вместо значения переменной
Список переменных окружения можно получить в терминале утилитой env.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <strings.h>

int main(int argc, char **argv, char **env)
{
	// Если нет аргументов для командной строки
	if (argc == 1)
	{
		printf("Usage: %s ENV_VAR1 [ENV_VAR2 ...]\n", argv[0]);
		printf("Prints the values of the specified environment variables.\n");
		printf("Variable names are case insensitive.\n");
		return 0;
	}

	// Перебирает все запрошенные переменные
	for (int i = 1; i < argc; i++)
	{
		char *var_name = argv[i];
		char *value = NULL;
		char *real_name = NULL;

		// Ищет переменную в окружении
		for (char **env_ptr = env; *env_ptr != NULL; env_ptr++)
		{
			char *env_var = *env_ptr;
			char *d = strchr(env_var, '=');

			if (d != NULL)
			{
				size_t name_len = d - env_var;
				if (strlen(var_name) == name_len &&
					strncasecmp(env_var, var_name, name_len) == 0)
				{
					real_name = strndup(env_var, name_len);
					value = d + 1;
					break;
				}
			}
		}

		printf("%s\n", real_name ? real_name : var_name); // Имя переменной
		printf("%s\n", value ? value : "");				  // Значение (или пустая строка)
		printf("---\n");								  // Разделитель

		if (real_name)
		{
			free(real_name);
		}
	}

	return 0;
}
