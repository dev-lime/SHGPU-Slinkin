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

int fieldWidth, fieldHeight;
unsigned char (*drawingField)[fieldWidth];

int CharToDigit(unsigned char c)
{
	return c - '0';
}

void PrintDrawingField()
{
	for (int row = 0; row < fieldHeight; row++)
	{
		for (int col = 0; col < fieldWidth; col++)
		{
			printf("%c", drawingField[row][col]);
		}
		printf("\n");
	}
}

void DrawHFractal(int centerX, int centerY, int segmentLength, int currentDepth, int maxRecursionDepth)
{
	if (currentDepth >= maxRecursionDepth)
	{
		return;
	}

	// Horizontal line
	for (int x = centerX; x <= centerX + round(segmentLength / 2); x++)
	{
		if (x >= 0 && x < fieldWidth)
		{
			drawingField[centerY][x] = '*';
		}
	}
	for (int x = centerX; x >= centerX - round(segmentLength / 2); x--)
	{
		if (x >= 0 && x < fieldWidth)
		{
			drawingField[centerY][x] = '*';
		}
	}

	int rightVerticalX = centerX + round(segmentLength / 2);
	int leftVerticalX = centerX - round(segmentLength / 2);
	int newSegmentLength = round(segmentLength / sqrt(2));

	// Right vertical line
	for (int y = centerY; y <= centerY + round(newSegmentLength / 2); y++)
	{
		if (y >= 0 && y < fieldHeight && rightVerticalX >= 0 && rightVerticalX < fieldWidth)
		{
			drawingField[y][rightVerticalX] = '*';
		}
	}
	for (int y = centerY; y >= centerY - round(newSegmentLength / 2); y--)
	{
		if (y >= 0 && y < fieldHeight && rightVerticalX >= 0 && rightVerticalX < fieldWidth)
		{
			drawingField[y][rightVerticalX] = '*';
		}
	}

	// Left vertical line
	for (int y = centerY; y <= centerY + round(newSegmentLength / 2); y++)
	{
		if (y >= 0 && y < fieldHeight && leftVerticalX >= 0 && leftVerticalX < fieldWidth)
		{
			drawingField[y][leftVerticalX] = '*';
		}
	}
	for (int y = centerY; y >= centerY - round(newSegmentLength / 2); y--)
	{
		if (y >= 0 && y < fieldHeight && leftVerticalX >= 0 && leftVerticalX < fieldWidth)
		{
			drawingField[y][leftVerticalX] = '*';
		}
	}

	DrawHFractal(centerX + round(segmentLength / 2), centerY + round(newSegmentLength / 2),
				 round(newSegmentLength / sqrt(2)), currentDepth + 1, maxRecursionDepth);
	DrawHFractal(centerX - round(segmentLength / 2), centerY - round(newSegmentLength / 2),
				 round(newSegmentLength / sqrt(2)), currentDepth + 1, maxRecursionDepth);
	DrawHFractal(centerX - round(segmentLength / 2), centerY + round(newSegmentLength / 2),
				 round(newSegmentLength / sqrt(2)), currentDepth + 1, maxRecursionDepth);
	DrawHFractal(centerX + round(segmentLength / 2), centerY - round(newSegmentLength / 2),
				 round(newSegmentLength / sqrt(2)), currentDepth + 1, maxRecursionDepth);
}

int main(int argc, char **argv)
{
	if (argc != 5)
	{
		printf("Usage: ./h_fractal field_width field_height initial_segment_length max_recursion_depth\n");
		return 1;
	}

	fieldWidth = atoi(argv[1]);
	fieldHeight = atoi(argv[2]);
	int initialSegmentLength = atoi(argv[3]);
	int maxRecursionDepth = atoi(argv[4]);

	drawingField = malloc(fieldHeight * sizeof(*drawingField));
	for (int i = 0; i < fieldHeight; i++)
	{
		drawingField[i] = malloc(fieldWidth * sizeof(**drawingField));
	}

	for (int row = 0; row < fieldHeight; row++)
	{
		for (int col = 0; col < fieldWidth; col++)
		{
			drawingField[row][col] = ' ';
		}
	}

	DrawHFractal(fieldWidth / 2, fieldHeight / 2, initialSegmentLength, 0, maxRecursionDepth);
	PrintDrawingField();

	for (int i = 0; i < fieldHeight; i++)
	{
		free(drawingField[i]);
	}
	free(drawingField);

	return 0;
}
