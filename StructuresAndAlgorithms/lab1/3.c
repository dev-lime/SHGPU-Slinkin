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

void draw_h(char **grid, int maxY, int maxX, int y, int x, int size, int depth, int current_depth)
{
	if (current_depth > depth)
	{
		return;
	}

	if (y < 0 || y >= maxY || x < 0 || x >= maxX)
	{
		return;
	}

	int half = size / 2;

	// Рисует вертикальные линии (|)
	for (int i = y - half; i <= y + half; i++)
	{
		if (i >= 0 && i < maxY)
		{
			// Левая линия
			if (x - half >= 0)
			{
				grid[i][x - half] = '*';
			}
			// Правая линия
			if (x + half < maxX)
			{
				grid[i][x + half] = '*';
			}
		}
	}

	// Рисует горизонтальную линию (-)
	for (int j = x - half; j <= x + half; j++)
	{
		if (j >= 0 && j < maxX)
		{
			grid[y][j] = '*';
		}
	}

	// Новый размер для следующего уровня рекурсии
	int new_size = (int)(size / sqrt(2));

	// Рекурсивно рисует 4 новые H в концах текущей H
	draw_h(grid, maxY, maxX, y - half, x - half, new_size, depth, current_depth + 1); // верхний левый
	draw_h(grid, maxY, maxX, y - half, x + half, new_size, depth, current_depth + 1); // верхний правый
	draw_h(grid, maxY, maxX, y + half, x - half, new_size, depth, current_depth + 1); // нижний левый
	draw_h(grid, maxY, maxX, y + half, x + half, new_size, depth, current_depth + 1); // нижний правый
}

int main(int argc, char *argv[])
{
	if (argc != 5)
	{
		fprintf(stderr, "Usage: %s MaxY MaxX Size Depth\n", argv[0]);
		return 1;
	}

	int maxY = atoi(argv[1]);
	int maxX = atoi(argv[2]);
	int size = atoi(argv[3]);
	int depth = atoi(argv[4]);

	// Выделяет память для сетки
	char **grid = (char **)malloc(maxY * sizeof(char *));
	for (int i = 0; i < maxY; i++)
	{
		grid[i] = (char *)malloc(maxX * sizeof(char));
		for (int j = 0; j < maxX; j++)
		{
			grid[i][j] = ' ';
		}
	}

	// Начинает построение с центра
	int centerY = maxY / 2;
	int centerX = maxX / 2;
	draw_h(grid, maxY, maxX, centerY, centerX, size, depth, 1);

	// Результат
	for (int i = 0; i < maxY; i++)
	{
		for (int j = 0; j < maxX; j++)
		{
			putchar(grid[i][j]);
		}
		putchar('\n');
	}

	// Освобождение памяти
	for (int i = 0; i < maxY; i++)
	{
		free(grid[i]);
	}
	free(grid);

	return 0;
}
