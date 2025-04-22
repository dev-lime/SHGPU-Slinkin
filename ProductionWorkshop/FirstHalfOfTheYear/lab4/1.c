/*
Дано натуральное значение N. Создать квадратную целочисленную матрицу N*N, заполнить ее нулями и предложить пользователю меню управления матрицей:
1. Вывести содержимое матрицы
2. Обнулить матрицу
3. Заполнить матрицу случайными значениями
4. Изменить элемент матрицы по его координатам
5. Повернуть матрицу на 90 градусов в указанном направлении
6. Транспонировать матрицу
0. Завершить работу с программой
Каждый элемент меню, кроме последнего, должен быть реализован в виде функции с передачей ему матрицы параметром вида int **m и размера матрицы N.
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Вывод матрицы
void printMatrix(int **m, int N)
{
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            printf("%4d ", m[i][j]);
        }
        printf("\n");
    }
}

// Обнуление матрицы
void zeroMatrix(int **m, int N)
{
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            m[i][j] = 0;
        }
    }
}

// Заполнение матрицы случайными значениями
void randomizeMatrix(int **m, int N)
{
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            m[i][j] = rand() % 100; // от 0 до 99
        }
    }
}

// Изменение элемента матрицы
void changeElement(int **m, int N)
{
    int i, j, value;
    printf("Coordinate: ");
    scanf("%d %d", &i, &j);
    if (i < 0 || i >= N || j < 0 || j >= N)
    {
        printf("Error\n");
        return;
    }
    printf("New value: ");
    scanf("%d", &value);
    m[i][j] = value;
}

// Поворот матрицы на 90 градусов
void rotateMatrix90(int **m, int N, int clockwise)
{
    int **temp = (int **)malloc(N * sizeof(int *));
    for (int i = 0; i < N; i++)
    {
        temp[i] = (int *)malloc(N * sizeof(int));
    }

    if (clockwise)
    { // по часовой
        for (int i = 0; i < N; i++)
        {
            for (int j = 0; j < N; j++)
            {
                temp[j][N - 1 - i] = m[i][j];
            }
        }
    }
    else
    { // против часовой
        for (int i = 0; i < N; i++)
        {
            for (int j = 0; j < N; j++)
            {
                temp[N - 1 - j][i] = m[i][j];
            }
        }
    }

    // Копирует результат обратно в исходную матрицу
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            m[i][j] = temp[i][j];
        }
    }

    // Освобождает временную матрицу
    for (int i = 0; i < N; i++)
    {
        free(temp[i]);
    }
    free(temp);
}

// Транспонирование матрицы
void transposeMatrix(int **m, int N)
{
    for (int i = 0; i < N; i++)
    {
        for (int j = i + 1; j < N; j++)
        {
            int temp = m[i][j];
            m[i][j] = m[j][i];
            m[j][i] = temp;
        }
    }
}

int main()
{
    srand(time(NULL)); // Инициализация генератора случайных чисел

    int N;
    printf("Введите размер матрицы N: ");
    scanf("%d", &N);

    // Выделение памяти под матрицу
    int **matrix = (int **)malloc(N * sizeof(int *));
    for (int i = 0; i < N; i++)
    {
        matrix[i] = (int *)malloc(N * sizeof(int));
    }

    // Инициализация матрицы нулями
    zeroMatrix(matrix, N);

    int choice;
    do
    {
        printf("\nМеню:\n");
        printf("1. Вывести содержимое матрицы\n");
        printf("2. Обнулить матрицу\n");
        printf("3. Заполнить матрицу случайными значениями\n");
        printf("4. Изменить элемент матрицы\n");
        printf("5. Повернуть матрицу на 90 градусов\n");
        printf("6. Транспонировать матрицу\n");
        printf("0. Выход\n");
        printf("Выберите пункт меню: ");
        scanf("%d", &choice);

        switch (choice)
        {
        case 1:
            printMatrix(matrix, N);
            break;
        case 2:
            zeroMatrix(matrix, N);
            printf("Матрица обнулена\n");
            break;
        case 3:
            randomizeMatrix(matrix, N);
            printf("Матрица заполнена случайными значениями\n");
            break;
        case 4:
            changeElement(matrix, N);
            break;
        case 5:
        {
            int direction;
            printf("Выберите направление (1 - по часовой, 0 - против): ");
            scanf("%d", &direction);
            rotateMatrix90(matrix, N, direction);
            printf("Матрица повернута\n");
            break;
        }
        case 6:
            transposeMatrix(matrix, N);
            printf("Матрица транспонирована\n");
            break;
        case 0:
            printf("Программа завершена\n");
            break;
        default:
            printf("Невозможный выбор\n");
        }
    } while (choice != 0);

    // Освобождение памяти
    for (int i = 0; i < N; i++)
    {
        free(matrix[i]);
    }
    free(matrix);

    return 0;
}
