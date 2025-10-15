/*
Выравнивание строки заключается в том, что между ее
отдельными словами  (см. задачу 269)  дополнительно вносятся
пробелы так, чтобы длина строки стала равной заданной длине
(предполагается, что требуемая длина не меньше исходной), а
последнее слово строки сдвинулось к ее правому краю. Составить
процедуру выравнивания заданной строки текста.
Решить задачу с помощью макроопределений. Полученные макроопределения должны вызываться как процедуры и не могут использоваться в выражениях.

269. Даны натуральное число n, символы s1, ..., sn. Группы
символов, разделенные пробелами (одним или несколькими) и не
содержащие пробелов внутри себя, будем называть словами.
а) Подсчитать количество слов в данной последовательности.
б) Подсчитать количество букв а в последнем слове данной
последовательности.
в) Найти количество слов, начинающихся с буквы б.
г) Найти количество слов, у которых первый и последний
символы совпадают между собой.
д) Найти какое-нибудь слово, начинающееся с буквы а.
е) Преобразовать данную последовательность, заменяя каждое
вхождение слова это на слово то.
ж) Найти длину самого короткого слова.

Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ALIGN_RIGHT(input, output, target_len)                                                         \
	do                                                                                                 \
	{                                                                                                  \
		const char *__input = (input);                                                                 \
		char **__output = (output);                                                                    \
		int __target_len = (target_len);                                                               \
		int __current_len = strlen(__input);                                                           \
		if (__target_len <= __current_len)                                                             \
		{                                                                                              \
			*__output = (char *)realloc(*__output, __current_len + 1);                                 \
			strcpy(*__output, __input);                                                                \
			break;                                                                                     \
		}                                                                                              \
		int __word_count = 0;                                                                          \
		int __in_word = 0;                                                                             \
		for (int i = 0; i < __current_len; i++)                                                        \
		{                                                                                              \
			if (__input[i] != ' ' && !__in_word)                                                       \
			{                                                                                          \
				__in_word = 1;                                                                         \
				__word_count++;                                                                        \
			}                                                                                          \
			else if (__input[i] == ' ')                                                                \
			{                                                                                          \
				__in_word = 0;                                                                         \
			}                                                                                          \
		}                                                                                              \
		if (__word_count <= 1)                                                                         \
		{                                                                                              \
			*__output = (char *)realloc(*__output, __target_len + 1);                                  \
			int __shift = __target_len - __current_len;                                                \
			memmove(*__output + __shift, __input, __current_len + 1);                                  \
			memset(*__output, ' ', __shift);                                                           \
			(*__output)[__target_len] = '\0';                                                          \
			break;                                                                                     \
		}                                                                                              \
		int __original_space_count = 0;                                                                \
		int __space_positions[__word_count - 1];                                                       \
		int __space_index = 0;                                                                         \
		__in_word = 1;                                                                                 \
		for (int i = 0; i < __current_len; i++)                                                        \
		{                                                                                              \
			if (__input[i] == ' ' && __in_word)                                                        \
			{                                                                                          \
				__in_word = 0;                                                                         \
				__space_positions[__space_index++] = i;                                                \
				__original_space_count++;                                                              \
			}                                                                                          \
			else if (__input[i] != ' ' && !__in_word)                                                  \
			{                                                                                          \
				__in_word = 1;                                                                         \
			}                                                                                          \
		}                                                                                              \
		int __total_spaces_to_add = __target_len - __current_len;                                      \
		int __base_spaces_to_add = __total_spaces_to_add / (__word_count - 1);                         \
		int __extra_spaces = __total_spaces_to_add % (__word_count - 1);                               \
		*__output = (char *)realloc(*__output, __target_len + 1);                                      \
		char *__result = *__output;                                                                    \
		int __input_pos = 0;                                                                           \
		int __output_pos = 0;                                                                          \
		__space_index = 0;                                                                             \
		for (int i = 0; i < __current_len; i++)                                                        \
		{                                                                                              \
			if (__space_index < __word_count - 1 && i == __space_positions[__space_index])             \
			{                                                                                          \
				__result[__output_pos++] = ' ';                                                        \
				__input_pos++;                                                                         \
				int __spaces_to_add = __base_spaces_to_add + (__space_index < __extra_spaces ? 1 : 0); \
				for (int j = 0; j < __spaces_to_add; j++)                                              \
				{                                                                                      \
					__result[__output_pos++] = ' ';                                                    \
				}                                                                                      \
				__space_index++;                                                                       \
			}                                                                                          \
			else                                                                                       \
			{                                                                                          \
				__result[__output_pos++] = __input[__input_pos++];                                     \
			}                                                                                          \
		}                                                                                              \
		__result[__output_pos] = '\0';                                                                 \
	} while (0)

int main()
{
	char input[1000];
	char *output = NULL;
	int target_len;

	printf("Введите строку: ");
	fgets(input, sizeof(input), stdin);
	input[strcspn(input, "\n")] = '\0';

	printf("Введите целевую длину строки: ");
	scanf("%d", &target_len);

	ALIGN_RIGHT(input, &output, target_len);

	printf("Выровненная строка:\n%s\n", output);
	printf("Длина строки: %zu\n", strlen(output));

	free(output);
	return 0;
}
