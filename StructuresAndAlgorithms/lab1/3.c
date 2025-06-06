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

int main(int argc, char **argv)
{
	int x = 0, y = 0, len = 0, iter = 0, maxN[4];

	int StrToInt(unsigned char c)
	{
		return c - 48;
	};

	if (argc > 4)
	{
		for (int j = 1; j <= 4; j++)
		{
			for (int i = 0; i <= pow(10,10); i++)
			{
				if (argv[j][i] == 0)
				{
					maxN[j - 1] = i;
					break;
				}
			}
		}

		for (int i = 0; i < maxN[0]; i++)
			x += pow(10,maxN[0] - 1 - i) * StrToInt(argv[1][i]);
		for (int i = 0; i < maxN[1]; i++)
			y += pow(10, maxN[1] - 1 - i) * StrToInt(argv[2][i]);
		for (int i = 0; i < maxN[2]; i++)
			len += pow(10, maxN[2] - 1 - i) * StrToInt(argv[3][i]);
		for (int i = 0; i < maxN[3]; i++)
			iter += pow(10, maxN[3] - 1 - i) * StrToInt(argv[4][i]);
	}
	else
	{
		printf("error\n");
		printf("./C2_1_3_herc x y len iter\n");
		return 0;
	}
	
	unsigned char array[y][x];

	for (int y1 = 0; y1 <= y - 1; y1++)
		for (int x1 = 0; x1 <= x - 1; x1++)
			array[y1][x1] = ' ';

	void outArray()
	{
		//printf("P1\n%d %d\n",x,y);
		for (int y1 = 0; y1 <= y - 1; y1++)
		{
			for (int x1 = 0; x1 <= x - 1; x1++)
				printf("%c", array[y1][x1]);
			printf("\n");
		}
	};

	void fill(int x0, int y0, int len0, int iter0)
	{
		if (iter0 >= iter)
		{
			return;
		}
		else
		{
			for (int x1 = x0; x1 <= x0 + round(len0 / 2); x1++)
				array[y0][x1] = '*';
			for (int x1 = x0; x1 >= x0 - round(len0 / 2); x1--)
				array[y0][x1] = '*';

			int x2 = x0 + round(len0 / 2), x3 = x0 - round(len0 / 2);
			int len1 = round(len0 / sqrt(2));
			
			for (int y1 = y0; y1 <= y0 + round(len1 / 2); y1++)
				array[y1][x2] = '*';
			for (int y1 = y0; y1 >= y0 - round(len1 / 2); y1--)
				array[y1][x2] = '*';

			for (int y1 = y0; y1 <= y0 + round(len1 / 2); y1++)
				array[y1][x3] = '*';
			for (int y1 = y0; y1 >= y0 - round(len1 / 2); y1--)
				array[y1][x3] = '*';

			fill(x0 + round(len0 / 2), y0 + round(len1 / 2), round(len1 / sqrt(2)), iter0 + 1);
			fill(x0 - round(len0 / 2), y0 - round(len1 / 2), round(len1 / sqrt(2)), iter0 + 1);
			fill(x0 - round(len0 / 2), y0 + round(len1 / 2), round(len1 / sqrt(2)), iter0 + 1);
			fill(x0 + round(len0 / 2), y0 - round(len1 / 2), round(len1 / sqrt(2)), iter0 + 1);
		}
	}

	fill(x / 2, y / 2, len, 0);
	outArray();
	return 0;
}
