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
                i--;
                M--;
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

    printf("Объединение a и b: ");
    for (int i = 0; i < M; i++) {
        printf("%d ", a[i]);
    }
    for (int i = 0; i < N; i++) {
        int is_in_a = 0;
        for (int j = 0; j < M; j++) {
            if (b[i] == a[j]) {
                is_in_a = 1;
                break;
            }
        }
        if (!is_in_a) {
            printf("%d ", b[i]);
        }
    }

    return 0;
}
