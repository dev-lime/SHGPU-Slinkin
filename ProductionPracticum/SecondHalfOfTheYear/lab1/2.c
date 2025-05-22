/*
Построение "ковра Серпинского".
Дано:
в первой строке одно натуральное значение - исходный размер стороны "ковра Серпинского",
во второй - количество итераций для получения результирующего фрактала,
в третьей - цвет исходного квадрата в формате RGB,
в четвертой - цвет изымаемых квадратов в формате RGB.

Найти:
сформировать "ковер Серпинского"

Требования и рекомендации:
Для решения задачи следует сформировать двумерный массив squares из необходимого количества структур
типа square_t (координаты левого верхнего угла и длина стороны), описывающих квадрат,
который будет изыматься из центров исходных квадратов на каждой итерации.
Рекомендуется использовать рекурсивное решение. Для хранения изображения следует использовать матрицу image,
элементы которой представляют собой структуру RGB для хранения цвета каждой точки.
Обработка сформированного массива squares должна обеспечить заполнение массива image.

Результат:
вывод полученного изображения из массива image на стандартный поток вывода в формате ppm P3.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct
{
    unsigned char r, g, b;
} RGB;

typedef struct
{
    int x, y; // левый верхний угол
    int size; // длина стороны
} square_t;

void generate_sierpinski(square_t **squares, int *count, int x, int y, int size, int iterations, int current_iteration)
{
    if (current_iteration >= iterations)
    {
        return;
    }

    int new_size = size / 3;
    int new_x = x + new_size;
    int new_y = y + new_size;

    (*count)++;
    *squares = (square_t *)realloc(*squares, *count * sizeof(square_t));
    (*squares)[*count - 1] = (square_t){new_x, new_y, new_size};

    // Рекурсивно обрабатывает 8 оставшихся квадратов
    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            if (i == 1 && j == 1)
                continue; // пропускает центральный квадрат

            int next_x = x + i * new_size;
            int next_y = y + j * new_size;
            generate_sierpinski(squares, count, next_x, next_y, new_size, iterations, current_iteration + 1);
        }
    }
}

void draw_squares(RGB **image, int image_size, square_t *squares, int squares_count, RGB base_color, RGB remove_color)
{
    for (int y = 0; y < image_size; y++)
    {
        for (int x = 0; x < image_size; x++)
        {
            image[y][x] = base_color;
        }
    }

    for (int i = 0; i < squares_count; i++)
    {
        square_t sq = squares[i];
        for (int y = sq.y; y < sq.y + sq.size && y < image_size; y++)
        {
            for (int x = sq.x; x < sq.x + sq.size && x < image_size; x++)
            {
                if (x >= 0 && y >= 0)
                {
                    image[y][x] = remove_color;
                }
            }
        }
    }
}

int main()
{
    int initial_size, iterations;
    RGB base_color, remove_color;

    scanf("%d", &initial_size);
    scanf("%d", &iterations);
    scanf("%hhu %hhu %hhu", &base_color.r, &base_color.g, &base_color.b);
    scanf("%hhu %hhu %hhu", &remove_color.r, &remove_color.g, &remove_color.b);

    int image_size = initial_size;

    RGB **image = (RGB **)malloc(image_size * sizeof(RGB *));
    for (int i = 0; i < image_size; i++)
    {
        image[i] = (RGB *)malloc(image_size * sizeof(RGB));
        for (int j = 0; j < image_size; j++)
        {
            image[i][j] = base_color; // заполняет базовым цветом
        }
    }

    // Генерирует квадраты для удаления
    square_t *squares = NULL;
    int squares_count = 0;

    generate_sierpinski(&squares, &squares_count, 0, 0, initial_size, iterations, 0);
    draw_squares(image, image_size, squares, squares_count, base_color, remove_color);

    printf("P3\n");
    printf("%d %d\n", image_size, image_size);
    printf("255\n");

    for (int y = 0; y < image_size; y++)
    {
        for (int x = 0; x < image_size; x++)
        {
            printf("%d %d %d ", image[y][x].r, image[y][x].g, image[y][x].b);
        }
        printf("\n");
    }

    for (int i = 0; i < image_size; i++)
    {
        free(image[i]);
    }
    free(image);
    free(squares);

    return 0;
}
