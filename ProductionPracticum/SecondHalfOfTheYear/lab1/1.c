/*
Построение "мозаики".
Дано:
в первой строке два натуральных значения через пробел - размер графического поля.
Во второй и последующих строках по семь натуральных значений - канонизированные
координаты прямоугольника и его цвет в формате RGB (RGB - это три байтовых числовых значения,
представляющих собой интенсивности красного (R), зеленого (G) и синего (B)).
Ввод завершает строка, состоящая из 7 нулей.

Найти:
сформировать "мозаику" - графическое изображение на белом фоне,
состоящее из разноцветных прямоугольников в однопискельных черных рамках.
Для этого исходные прямоугольники поледовательно, в порядке поступления,
накладываются на изображение, начиная с прямоугольника - графического поля.

Требования и рекомендации:
Для построения изображения следует на основе исходных данных сформировать массив rects,
который будет содержать структуры типа rect_t (координаты левого верхнего и правого нижнего углов),
исходных прямоугольников. Первым прямоугольником считать графическое поле.
Для хранения изображения следует использовать матрицу image, элементы которой
представляют собой структуру RGB для хранения цвета каждой точки.
Обработка сформированного массива rects должна обеспечить заполнение массива image.

Результат:
вывод полученного изображения из массива image на стандартный поток вывода в формате ppm P3.
*/
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
    unsigned char r, g, b;
} RGB;

typedef struct
{
    int x1, y1; // левый верхний угол
    int x2, y2; // правый нижний угол
    RGB color;
} rect_t;

void draw_rectangle(RGB **image, int width, int height,
                    int x1, int y1, int x2, int y2, RGB color)
{
    int left = x1 < x2 ? x1 : x2;
    int right = x1 < x2 ? x2 : x1;
    int top = y1 < y2 ? y1 : y2;
    int bottom = y1 < y2 ? y2 : y1;

    left = left < 0 ? 0 : left;
    right = right >= width ? width - 1 : right;
    top = top < 0 ? 0 : top;
    bottom = bottom >= height ? height - 1 : bottom;

    // Рисует прямоугольник
    for (int y = top; y <= bottom; y++)
    {
        for (int x = left; x <= right; x++)
        {
            image[y][x] = color;
        }
    }
}

void draw_rectangle_with_border(RGB **image, int width, int height,
                                int x1, int y1, int x2, int y2, RGB color)
{
    draw_rectangle(image, width, height, x1 - 1, y1 - 1, x2 + 1, y2 + 1, (RGB){0, 0, 0});
    draw_rectangle(image, width, height, x1, y1, x2, y2, color);
}

int main()
{
    int width, height;
    scanf("%d %d", &width, &height);

    // Создаёт изображение с белым фоном
    RGB **image = (RGB **)malloc(height * sizeof(RGB *));
    for (int i = 0; i < height; i++)
    {
        image[i] = (RGB *)malloc(width * sizeof(RGB));
        for (int j = 0; j < width; j++)
        {
            image[i][j] = (RGB){255, 255, 255}; // белый фон
        }
    }

    // Массив прямоугольников
    rect_t *rects = NULL;
    int rect_count = 0;

    // Чтение прямоугольников
    while (1)
    {
        int x1, y1, x2, y2, r, g, b;
        scanf("%d %d %d %d %d %d %d", &x1, &y1, &x2, &y2, &r, &g, &b);

        if (x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0 && r == 0 && g == 0 && b == 0)
        {
            break;
        }

        // Добавление прямоугольника в массив
        rect_count++;
        rects = (rect_t *)realloc(rects, rect_count * sizeof(rect_t));
        rects[rect_count - 1] = (rect_t){x1, y1, x2, y2, (RGB){(unsigned char)r, (unsigned char)g, (unsigned char)b}};
    }

    // Отрисовка всех прямоугольников
    for (int r = 0; r < rect_count; r++)
    {
        draw_rectangle_with_border(image, width, height,
                                   rects[r].x1, rects[r].y1,
                                   rects[r].x2, rects[r].y2,
                                   rects[r].color);
    }

    // Вывод изображения в формате PPM P3
    printf("P3\n");
    printf("%d %d\n", width, height);
    printf("255\n");

    for (int y = 0; y < height; y++)
    {
        for (int x = 0; x < width; x++)
        {
            printf("%d %d %d ", image[y][x].r, image[y][x].g, image[y][x].b);
        }
        printf("\n");
    }

    // Освобождение памяти
    for (int i = 0; i < height; i++)
    {
        free(image[i]);
    }
    free(image);
    free(rects);

    return 0;
}
