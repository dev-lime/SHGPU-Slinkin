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

int charToDigit(char c)
{
	return c - '0';
}

int stringToInt(char *str, int length)
{
	int result = 0;
	for (int i = 0; i < length; i++)
	{
		result = result * 10 + charToDigit(str[i]);
	}
	return result;
}

void printArray(unsigned char **array, int width, int height)
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

void buildHFractal(unsigned char **array, int centerX, int centerY,
				   int length, int currentIter, int maxIter)
{
	if (currentIter >= maxIter)
	{
		return;
	}

	// Рисует горизонтальную линию H
	int halfLength = round(length / 2.0);
	for (int x = centerX - halfLength; x <= centerX + halfLength; x++)
	{
		if (x >= 0 && x < length * 2)
		{ // Проверка границ
			array[centerY][x] = '*';
		}
	}

	// Вычисляет длину вертикальных линий
	int verticalLength = round(length / sqrt(2));

	// Рисует правую вертикальную линию H
	int rightX = centerX + halfLength;
	for (int y = centerY - verticalLength / 2; y <= centerY + verticalLength / 2; y++)
	{
		if (y >= 0 && y < length * 2)
		{ // Проверка границ
			array[y][rightX] = '*';
		}
	}

	// Рисует левую вертикальную линию H
	int leftX = centerX - halfLength;
	for (int y = centerY - verticalLength / 2; y <= centerY + verticalLength / 2; y++)
	{
		if (y >= 0 && y < length * 2)
		{ // Проверка границ
			array[y][leftX] = '*';
		}
	}

	// Рекурсивно строит H-фракталы в 4 углах
	int newLength = round(verticalLength / sqrt(2));
	buildHFractal(array, rightX, centerY + verticalLength / 2, newLength, currentIter + 1, maxIter);
	buildHFractal(array, leftX, centerY - verticalLength / 2, newLength, currentIter + 1, maxIter);
	buildHFractal(array, leftX, centerY + verticalLength / 2, newLength, currentIter + 1, maxIter);
	buildHFractal(array, rightX, centerY - verticalLength / 2, newLength, currentIter + 1, maxIter);
}

int main(int argc, char **argv)
{
	if (argc != 5)
	{
		printf("Ошибка: неверное количество аргументов\n");
		printf("Использование: %s width height initial_length max_iterations\n", argv[0]);
		return 1;
	}

	// Парсинг аргументов командной строки
	int width = atoi(argv[1]);
	int height = atoi(argv[2]);
	int initialLength = atoi(argv[3]);
	int maxIterations = atoi(argv[4]);

	// Выделение памяти для массива
	unsigned char **array = (unsigned char **)malloc(height * sizeof(unsigned char *));
	for (int y = 0; y < height; y++)
	{
		array[y] = (unsigned char *)malloc(width * sizeof(unsigned char));
		for (int x = 0; x < width; x++)
		{
			array[y][x] = ' ';
		}
	}

	// Построение фрактала
	int centerX = width / 2;
	int centerY = height / 2;
	buildHFractal(array, centerX, centerY, initialLength, 0, maxIterations);

	printArray(array, width, height);

	for (int y = 0; y < height; y++)
	{
		free(array[y]);
	}
	free(array);

	return 0;
}
