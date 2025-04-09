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

    printf("Объединение a и b: ");

    // Сначала выводим уникальные элементы из a
    for (int i = 0; i < M; i++) {
        int is_duplicate = 0;
        for (int j = 0; j < i; j++) {
            if (a[j] == a[i]) {
                is_duplicate = 1;
                break;
            }
        }
        if (!is_duplicate) {
            printf("%d ", a[i]);
        }
    }

    // Затем добавляем элементы из b, которых нет в a
    for (int i = 0; i < N; i++) {
        int is_in_a = 0;
        for (int j = 0; j < M; j++) {
            if (b[i] == a[j]) {
                is_in_a = 1;
                break;
            }
        }
        if (!is_in_a) {
            // Проверяем, не дублируется ли b[i] в уже выведенных b
            int is_duplicate_in_b = 0;
            for (int j = 0; j < i; j++) {
                if (b[j] == b[i]) {
                    is_duplicate_in_b = 1;
                    break;
                }
            }
            if (!is_duplicate_in_b) {
                printf("%d ", b[i]);
            }
        }
    }

    return 0;
}
