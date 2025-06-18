/*
Разработать программу построения H-дерева (H-фрактала).
Принцип построения заключается в формировании набора состыкованных центрами,
перпендикулярных друг другу отрезков, длина каждого из которых в корень квадратный раз меньше длины предыдущего.
Основная функция должна строить набор из трех таких отрезков в виде буквы H и рекурсивно вызывать саму себя
для построения следующих троек.
Параметры функции:
1) координаты (Y,X) центра буквы H,
2) размер горизонтальной перекладины,
3) текущая клубина рекурсии.
На вход программа принимает размеры (MaxY,MaxX) прямоугольного поля, первоначальный размер горизонтального отрезка,
максимальную глубину рекурсии. Программа выводит набор строк,
где знаками "*" отмечен H-фрактал, построение начинается с центра поля.
Для проверки корректности собственных решений можно использовать программы hrec и pbm.
hrec строит H-фрактал, а pbm переводит его в графический формат.
Программы скомпилированы для платформы Linux x86-64.
Пример генерации рисунка фрактала:
./hrec 800 800 400 7 | ./pbm > result.pbm
В данном примере в центре поля размером 800 на 800 точек генерируется H-фрактал
с размером первоначального горизонтального отрезка в 400 точек.
Максимальная глубина рекурсии равна 7. Полученный набор строк конвееризируется на вход программы pbm,
которая формирует рисунок и сохраняет его в графическом файле result.pbm (формат PBM, версия P1)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

unsigned char **array;
int width, height;

int stringToNumber(char *str)
{
	int result = 0;
	for (int i = 0; str[i] != '\0'; i++)
	{
		result = result * 10 + (str[i] - '0');
	}
	return result;
}

// Рекурсивная функция построения H-фрактала
void buildH(int x0, int y0, int len, int iteration, int maxIterations)
{
	if (iteration >= maxIterations)
		return;

	// Рисует горизонтальную линию
	int halfLen = len / 2;
	for (int x = x0 - halfLen; x <= x0 + halfLen; x++)
	{
		if (x >= 0 && x < width)
		{
			array[y0][x] = '*';
		}
	}

	// Вычисляет длину вертикальных линий
	int verticalLen = len / sqrt(2);

	// Рисует вертикальные линии
	int halfVert = verticalLen / 2;
	for (int y = y0 - halfVert; y <= y0 + halfVert; y++)
	{
		if (y >= 0 && y < height)
		{
			// Правая вертикаль
			if (x0 + halfLen >= 0 && x0 + halfLen < width)
			{
				array[y][x0 + halfLen] = '*';
			}
			// Левая вертикаль
			if (x0 - halfLen >= 0 && x0 - halfLen < width)
			{
				array[y][x0 - halfLen] = '*';
			}
		}
	}

	// Рекурсия
	int newLen = verticalLen / sqrt(2);
	buildH(x0 + halfLen, y0 + halfVert, newLen, iteration + 1, maxIterations); // Верхний правый
	buildH(x0 - halfLen, y0 - halfVert, newLen, iteration + 1, maxIterations); // Нижний левый
	buildH(x0 - halfLen, y0 + halfVert, newLen, iteration + 1, maxIterations); // Верхний левый
	buildH(x0 + halfLen, y0 - halfVert, newLen, iteration + 1, maxIterations); // Нижний правый
}

int main(int argc, char **argv)
{
	// Проверка
	if (argc != 5)
	{
		printf("Ошибка: неверное количество аргументов\n");
		printf("Использование: %s width height initial_length max_iterations\n", argv[0]);
		return 1;
	}

	// Аргументы
	width = stringToNumber(argv[1]);
	height = stringToNumber(argv[2]);
	int initialLength = stringToNumber(argv[3]);
	int maxIterations = stringToNumber(argv[4]);

	// Создание массива
	array = (unsigned char **)malloc(height * sizeof(unsigned char *));
	for (int y = 0; y < height; y++)
	{
		array[y] = (unsigned char *)malloc(width * sizeof(unsigned char));
		for (int x = 0; x < width; x++)
		{
			array[y][x] = ' ';
		}
	}

	// Запуск построения фрактала из центра
	buildH(width / 2, height / 2, initialLength, 0, maxIterations);

	// Вывод
	for (int y = 0; y < height; y++)
	{
		for (int x = 0; x < width; x++)
		{
			printf("%c", array[y][x]);
		}
		printf("\n");
	}

	for (int y = 0; y < height; y++)
	{
		free(array[y]);
	}
	free(array);

	return 0;
}
