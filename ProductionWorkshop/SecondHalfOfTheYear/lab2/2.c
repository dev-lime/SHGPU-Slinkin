/*
Обработка файла растрового изображения (высокоуровневая или низкоуровневая обработка файла)
Дан файл формата BMP, представляющий собой квадратное изображение. Создать программу, которая загружает из переданного ей файла формата BMP заголовки формата, анализирует их, а затем заменяет в исходном файле только те байты, которые позволяют превратить изображение 1 в изображение 2. ЗАПРЕЩЕНА загрузка самого изображения в оперативную память с последующей обработкой.
Изображение 1 - оригинальное изображение
Изображение 2 - перечёркнуторе по диагонали (из угла в угол) изображение
При проверке решения студент имеет право зафиксировать любые параметры формата (версию, палитру, метод кодирования и т.п.). Преподаватель имеет право произвольно задавать содержимое и размер квадратного изображения, а также цвет пересекающихся отрезков.
*/

/*
BMP 24-бит без сжатия (BI_RGB).
Квадратное изображение.
Один пиксель = 3 байта (RGB).
Без палитры.
*/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#pragma pack(push, 1) // без выравнивания
typedef struct
{
    uint16_t bfType;
    uint32_t bfSize;
    uint16_t bfReserved1;
    uint16_t bfReserved2;
    uint32_t bfOffBits;
} BITMAPFILEHEADER;

typedef struct
{
    uint32_t biSize;
    int32_t biWidth;
    int32_t biHeight;
    uint16_t biPlanes;
    uint16_t biBitCount;
    uint32_t biCompression;
    uint32_t biSizeImage;
    int32_t biXPelsPerMeter;
    int32_t biYPelsPerMeter;
    uint32_t biClrUsed;
    uint32_t biClrImportant;
} BITMAPINFOHEADER;
#pragma pack(pop)

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s input.bmp\n", argv[0]);
        return 1;
    }

    FILE *bmp = fopen(argv[1], "rb+");
    if (!bmp)
    {
        perror("Cannot open file");
        return 1;
    }

    BITMAPFILEHEADER fileHeader;
    fread(&fileHeader, sizeof(fileHeader), 1, bmp);

    if (fileHeader.bfType != 0x4D42)
    {
        printf("Not a BMP file\n");
        fclose(bmp);
        return 1;
    }

    BITMAPINFOHEADER infoHeader;
    fread(&infoHeader, sizeof(infoHeader), 1, bmp);

    if (infoHeader.biBitCount != 24 || infoHeader.biCompression != 0)
    {
        printf("Only 24-bit uncompressed BMP supported\n");
        fclose(bmp);
        return 1;
    }

    int width = infoHeader.biWidth;
    int height = infoHeader.biHeight;
    if (width != height)
    {
        printf("Only square images supported\n");
        fclose(bmp);
        return 1;
    }

    int rowSize = (width * 3 + 3) & ~3;     // выравнивание по 4 байта
    uint8_t red = 255, green = 0, blue = 0; // цвет диагонали — красный

    for (int y = 0; y < height; y++)
    {
        int x1 = y;             // первая диагональ (верхний левый -> нижний правый)
        int x2 = width - 1 - y; // вторая диагональ (верхний правый -> нижний левый)

        if (x1 == x2)
        {
            // центр один раз
            long pixelOffset = fileHeader.bfOffBits + (height - 1 - y) * rowSize + x1 * 3;
            fseek(bmp, pixelOffset, SEEK_SET);
            fwrite(&blue, 1, 1, bmp);  // B
            fwrite(&green, 1, 1, bmp); // G
            fwrite(&red, 1, 1, bmp);   // R
        }
        else
        {
            int positions[2] = {x1, x2};
            for (int i = 0; i < 2; ++i)
            {
                int x = positions[i];
                long pixelOffset = fileHeader.bfOffBits + (height - 1 - y) * rowSize + x * 3;
                fseek(bmp, pixelOffset, SEEK_SET);
                fwrite(&blue, 1, 1, bmp);
                fwrite(&green, 1, 1, bmp);
                fwrite(&red, 1, 1, bmp);
            }
        }
    }

    fclose(bmp);
    printf("Image diagonals successfully\n");
    return 0;
}
