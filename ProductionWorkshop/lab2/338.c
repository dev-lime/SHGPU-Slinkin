#include <stdio.h>

int main() {
    int M, N, a, b, i, j;
    scanf("%d %d", &M, &N);

    for (i = 0; i < M; i++) {
        scanf("%d", &a);
        for (j = 0; j < N; j++) {
            scanf("%d", &b);
            if (a == b) printf("%d ", a);
        }
    }
    printf("\n");
    return 0;
}
