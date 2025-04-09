#include <stdio.h>

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

    printf("Элементы b, не входящие в a: ");
    int has_difference = 0;

    for (int i = 0; i < N; i++) {
        int is_in_a = 0;
        for (int j = 0; j < M; j++) {
            if (b[i] == a[j]) {
                is_in_a = 1;
                break;
            }
        }
        if (!is_in_a) {
            // Проверяем, не дублируется ли b[i] в уже выведенных
            int is_duplicate = 0;
            for (int j = 0; j < i; j++) {
                if (b[j] == b[i]) {
                    is_duplicate = 1;
                    break;
                }
            }
            if (!is_duplicate) {
                printf("%d ", b[i]);
                has_difference = 1;
            }
        }
    }

    if (!has_difference) {
        printf("Таких элементов нет");
    }

    return 0;
}
