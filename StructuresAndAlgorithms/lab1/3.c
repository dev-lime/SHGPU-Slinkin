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

#define EMPTY_CELL ' '
#define FILLED_CELL '*'

void initializeArray(unsigned char **array, int height, int width)
{
	for (int y = 0; y < height; y++)
	{
		for (int x = 0; x < width; x++)
		{
			array[y][x] = EMPTY_CELL;
		}
	}
}

void printArray(unsigned char **array, int height, int width)
{
	for (int y = 0; y < height; y++)
	{
		for (int x = 0; x < width; x++)
		{
			printf("%c", array[y][x]);
		}
		printf("\n");
	}
}

void drawH(unsigned char **array, int centerX, int centerY, int length, int currentDepth, int maxDepth)
{
	if (currentDepth >= maxDepth)
	{
		return;
	}

	// Draw horizontal line
	int halfLength = length / 2;
	for (int x = centerX - halfLength; x <= centerX + halfLength; x++)
	{
		array[centerY][x] = FILLED_CELL;
	}

	// Draw vertical lines
	int newLength = length / sqrt(2);
	int newHalfLength = newLength / 2;

	for (int y = centerY - newHalfLength; y <= centerY + newHalfLength; y++)
	{
		array[y][centerX + halfLength] = FILLED_CELL; // Right vertical
		array[y][centerX - halfLength] = FILLED_CELL; // Left vertical
	}

	// Recursively draw smaller H's at four corners
	drawH(array, centerX + halfLength, centerY + newHalfLength, newLength, currentDepth + 1, maxDepth);
	drawH(array, centerX - halfLength, centerY - newHalfLength, newLength, currentDepth + 1, maxDepth);
	drawH(array, centerX - halfLength, centerY + newHalfLength, newLength, currentDepth + 1, maxDepth);
	drawH(array, centerX + halfLength, centerY - newHalfLength, newLength, currentDepth + 1, maxDepth);
}

int main(int argc, char **argv)
{
	if (argc != 5)
	{
		printf("Usage: %s width height length max_depth\n", argv[0]);
		return 1;
	}

	int width = atoi(argv[1]);
	int height = atoi(argv[2]);
	int initialLength = atoi(argv[3]);
	int maxDepth = atoi(argv[4]);

	// Allocate 2D array
	unsigned char **array = malloc(height * sizeof(unsigned char *));
	for (int y = 0; y < height; y++)
	{
		array[y] = malloc(width * sizeof(unsigned char));
	}

	initializeArray(array, height, width);
	drawH(array, width / 2, height / 2, initialLength, 0, maxDepth);
	printArray(array, height, width);

	// Free allocated memory
	for (int y = 0; y < height; y++)
	{
		free(array[y]);
	}
	free(array);

	return 0;
}
