#include <stdio.h>

int main() {
    int M, N;

    printf("Введите M: ");
    scanf("%d", &M);
    int a[M];
    printf("Введите %d чисел a1..aM: ", M);
    for (int i = 0; i < M; i++) {
        scanf("%d", &a[i]);
        for (int j = 0; j < i; j++) {
            if (a[j] == a[i]) {
                i--; // Повторяет ввод элемента
                M--; // Уменьшает размер массива
                break;
            }
        }
    }

    printf("Введите N: ");
    scanf("%d", &N);
    int b[N];
    printf("Введите %d чисел b1..bN: ", N);
    for (int i = 0; i < N; i++) {
        scanf("%d", &b[i]);
        for (int j = 0; j < i; j++) {
            if (b[j] == b[i]) {
                i--;
                N--;
                break;
            }
        }
    }

    printf("Пересечение a и b: ");
    int has_intersection = 0;
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < N; j++) {
            if (a[i] == b[j]) {
                printf("%d ", a[i]);
                has_intersection = 1;
                break;
            }
        }
    }

    if (!has_intersection) {
        printf("Пересечение пусто");
    }

    printf("\n");
    return 0;
}
