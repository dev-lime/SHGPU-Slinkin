#include <stdio.h>

int main()
{
    int n;
    scanf("%d", &n);

    for (int x = 0; x*x <= n; x++) {
        int sum1 = x*x;
        for (int y = x; sum1 + y*y <= n; y++) {
            int sum2 = sum1 + y*y;
            for (int z = y; sum2 + z*z <= n; z++) {
                int sum3 = sum2 + z*z;
                for (int t = z; sum3 + t*t <= n; t++) {
                    if (sum3 + t*t == n) {
                        printf("%d %d %d %d\n", x, y, z, t);
                    }
                }
            }
        }
    }

    return 0;
}
