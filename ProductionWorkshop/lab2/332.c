#include <stdio.h>

int main()
{
    int n;
    scanf("%d", &n);

    for (int x = 0; x*x <= n; x++)
        for (int y = x; y*y + x*x <= n; y++)
            for (int z = y; z*z + y*y + x*x <= n; z++)
                for (int t = z; t*t + z*z + y*y + x*x <= n; t++)
                    if (x*x + y*y + z*z + t*t == n)
                        printf("%d %d %d %d\n", x, y, z, t);

    return 0;
}
