/*
Даны действительные положительные числа x, y, z.
а) Выяснить существует ли треугольник с длинами сторон x, y, z.
б) Если треугольник существует, то ответить – является ли он
остроугольным.
Решить задачу, с помощью функций и макроопределений, предназначенных для использования в выражениях.
Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

/*
Даны действительные положительные числа x, y, z.
а) Выяснить существует ли треугольник с длинами сторон x, y, z.
б) Если треугольник существует, то ответить – является ли он
остроугольным.
Решить задачу, с помощью функций и макроопределений, предназначенных для использования в выражениях.
Создаваемые функции и макроопределения не должны обращаться к внешним для них переменным.
*/

#include <stdio.h>
#include <math.h>

#define IS_TRIANGLE(x, y, z) ((x) + (y) > (z) && (x) + (z) > (y) && (y) + (z) > (x))
#define IS_ACUTE_ANGLE(x, y, z) (                                                                                                       \
	((x) >= (y) && (x) >= (z)) ? ((y) * (y) + (z) * (z) > (x) * (x)) : ((y) >= (x) && (y) >= (z)) ? ((x) * (x) + (z) * (z) > (y) * (y)) \
																								  : ((x) * (x) + (y) * (y) > (z) * (z)))

int is_triangle(double x, double y, double z)
{
	return x + y > z && x + z > y && y + z > x;
}

int is_acute_triangle(double x, double y, double z)
{
	double max_side = x;
	if (y > max_side)
		max_side = y;
	if (z > max_side)
		max_side = z;

	if (max_side == x)
	{
		return y * y + z * z > x * x;
	}
	else if (max_side == y)
	{
		return x * x + z * z > y * y;
	}
	else
	{
		return x * x + y * y > z * z;
	}
}

int main()
{
	double x, y, z;

	printf("x, y, z: ");
	scanf("%lf %lf %lf", &x, &y, &z);

	if (x <= 0 || y <= 0 || z <= 0)
	{
		return 1;
	}

	if (IS_TRIANGLE(x, y, z))
	{
		printf("M Треугольник существует\n");

		if (IS_ACUTE_ANGLE(x, y, z))
		{
			printf("M Остроугольный\n");
		}
		else
		{
			printf("M Треугольник не остроугольный\n");
		}
	}
	else
	{
		printf("M Треугольник не существует\n");
	}

	if (is_triangle(x, y, z))
	{
		printf("F Треугольник существует\n");

		if (is_acute_triangle(x, y, z))
		{
			printf("F Остроугольный\n");
		}
		else
		{
			printf("F Треугольник не остроугольный\n");
		}
	}
	else
	{
		printf("F Треугольник не существует\n");
	}

	return 0;
}
