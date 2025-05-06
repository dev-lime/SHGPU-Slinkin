/*
Выравнивание строки заключается в том, что между ее
отдельными словами  (см. задачу 269)  дополнительно вносятся
пробелы так, чтобы длина строки стала равной заданной длине
(предполагается, что требуемая длина не меньше исходной), а
последнее слово строки сдвинулось к ее правому краю. Составить
процедуру выравнивания заданной строки текста.
Решить задачу с помощью функций типа void.

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

void align_right(const char *input, char **output, int target_len)
{
	int current_len = strlen(input);
	if (target_len <= current_len)
	{
		*output = (char *)realloc(*output, current_len + 1);
		strcpy(*output, input);
		return;
	}

	// Счётчик слов
	int word_count = 0;
	int in_word = 0;
	for (int i = 0; i < current_len; i++)
	{
		if (input[i] != ' ' && !in_word)
		{
			in_word = 1;
			word_count++;
		}
		else if (input[i] == ' ')
		{
			in_word = 0;
		}
	}

	// Если одно слово ИЛИ пустая строка -> сдвигаем вправо
	if (word_count <= 1)
	{
		*output = (char *)realloc(*output, target_len + 1);
		int shift = target_len - current_len;
		memmove(*output + shift, input, current_len + 1);
		memset(*output, ' ', shift);
		(*output)[target_len] = '\0';
		return;
	}

	// Счётчик пробелов
	int original_space_count = 0;
	int space_positions[word_count - 1];
	int space_index = 0;

	in_word = 1;
	for (int i = 0; i < current_len; i++)
	{
		if (input[i] == ' ' && in_word)
		{
			in_word = 0;
			space_positions[space_index++] = i;
			original_space_count++;
		}
		else if (input[i] != ' ' && !in_word)
		{
			in_word = 1;
		}
	}

	int total_spaces_to_add = target_len - current_len;
	int base_spaces_to_add = total_spaces_to_add / (word_count - 1);
	int extra_spaces = total_spaces_to_add % (word_count - 1);

	*output = (char *)realloc(*output, target_len + 1);
	char *result = *output;

	int input_pos = 0;
	int output_pos = 0;
	space_index = 0;

	for (int i = 0; i < current_len; i++)
	{
		if (space_index < word_count - 1 && i == space_positions[space_index])
		{
			result[output_pos++] = ' ';
			input_pos++;

			int spaces_to_add = base_spaces_to_add + (space_index < extra_spaces ? 1 : 0);
			for (int j = 0; j < spaces_to_add; j++)
			{
				result[output_pos++] = ' ';
			}

			space_index++;
		}
		else
		{
			result[output_pos++] = input[input_pos++];
		}
	}
	result[output_pos] = '\0';
}

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

	align_right(input, &output, target_len);

	printf("Выровненная строка:\n%s\n", output);
	printf("Длина строки: %zu\n", strlen(output));

	free(output);
	return 0;
}
