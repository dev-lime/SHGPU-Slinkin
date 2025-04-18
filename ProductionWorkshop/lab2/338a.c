#include <stdio.h>
/// СДАНО ДО СЮДА, ДАЛЬШЕ ПУТИ НЕТ!
int main() {
    int M, N;
    
    printf("Введите M: ");
    scanf("%d", &M);
    int a[M];
    printf("Введите %d чисел a1..aM: ", M);
    for (int i = 0; i < M; i++) {
        scanf("%d", &a[i]);
    }

    printf("Введите N: ");
    scanf("%d", &N);
    int b[N];
    printf("Введите %d чисел b1..bN: ", N);
    for (int i = 0; i < N; i++) {
        scanf("%d", &b[i]);
    }

    printf("Пересечение a и b: ");
    int has_intersection = 0;

    for (int i = 0; i < M; i++) {
        // Проверяем, есть ли a[i] в b
        for (int j = 0; j < N; j++) {
            if (a[i] == b[j]) {
                // Проверяем, не выводили ли уже это число
                int is_duplicate = 0;
                for (int k = 0; k < i; k++) {
                    if (a[k] == a[i]) {
                        is_duplicate = 1;
                        break;
                    }
                }
                if (!is_duplicate) {
                    printf("%d ", a[i]);
                    has_intersection = 1;
                }
                break;
            }
        }
    }

    if (!has_intersection) {
        printf("Пересечение пусто");
    }

    return 0;
}
