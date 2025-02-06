#include <stdio.h>
#include <math.h>

int main() {
    int N, num, temp, sum, digit, count;
    scanf("%d", &N);

    int start = pow(10, N - 1), end = pow(10, N);
    for (num = start; num < end; num++) {
        temp = num;
        sum = 0;
        count = N;
        while (temp) {
            digit = temp % 10;
            sum += pow(digit, count);
            temp /= 10;
        }
        if (sum == num) printf("%d\n", num);
    }

    return 0;
}
