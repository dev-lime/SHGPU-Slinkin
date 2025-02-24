#include <stdio.h>
#include <limits.h>

int main() {
    signed char X_signed;
    signed char Y_signed;
    unsigned char X_unsigned;
    unsigned char Y_unsigned;

    printf("Signed X: ");
    scanf("%hhd", &X_signed);
    printf("Signed Y: ");
    scanf("%hhd", &Y_signed);
    printf("Unsigned X: ");
    scanf("%hhu", &X_unsigned);
    printf("Unsigned Y: ");
    scanf("%hhu", &Y_unsigned);

    int operations_add, operations_sub;
    char original_X;

    original_X = X_signed;
    operations_add = 0;
    while (1) {
        char new_X = X_signed + Y_signed;
        if ((new_X < X_signed && Y_signed > 0) || (new_X > X_signed && Y_signed < 0)) {
            break;
        }
        X_signed = new_X;
        operations_add++;
    }
	operations_add++;
    X_signed = original_X;
    operations_sub = 0;
    while (1) {
        char new_X = X_signed - Y_signed;
        if ((new_X > X_signed && Y_signed > 0) || (new_X < X_signed && Y_signed < 0)) {
            break;
        }
        X_signed = new_X;
        operations_sub++;
    }
	operations_sub++;
    printf("\nX is signed, Y is signed\n");
    printf("Operations X = X + Y: %d\n", operations_add);
    printf("Operations X = X - Y: %d\n", operations_sub);
    printf("\n");

    /*original_X = X_signed;
    operations_add = 0;
    while (1) {
        char new_X = X_signed + Y_unsigned;
        if (new_X < X_signed) {
            break;
        }
        X_signed = new_X;
        operations_add++;
    }
	operations_add++;
    X_signed = original_X;
    operations_sub = 0;
    while (1) {
        char new_X = X_signed - Y_unsigned;
        if (new_X > X_signed) {
            break;
        }
        X_signed = new_X;
        operations_sub++;
    }
	operations_sub++;
    printf("X is signed, Y is unsigned\n");
    printf("Operations X = X + Y: %d\n", operations_add);
    printf("Operations X = X - Y: %d\n", operations_sub);
    printf("\n");*/

    original_X = X_unsigned;
    operations_add = 0;
    while (1) {
        unsigned char new_X = X_unsigned + Y_signed;
        if ((new_X < X_unsigned && Y_signed > 0) || (new_X > X_unsigned && Y_signed < 0)) {
            break;
        }
        X_unsigned = new_X;
        operations_add++;
    }
	operations_add++;
    X_unsigned = original_X;
    operations_sub = 0;
    while (1) {
        unsigned char new_X = X_unsigned - Y_signed;
        if ((new_X > X_unsigned && Y_signed > 0) || (new_X < X_unsigned && Y_signed < 0)) {
            break;
        }
        X_unsigned = new_X;
        operations_sub++;
    }
	operations_sub++;
    printf("X is unsigned, Y is signed\n");
    printf("Operations X = X + Y: %d\n", operations_add);
    printf("Operations X = X - Y: %d\n", operations_sub);
    printf("\n");

    original_X = X_unsigned;
    operations_add = 0;
    while (1) {
        unsigned char new_X = X_unsigned + Y_unsigned;
        if (new_X < X_unsigned) {
            break;
        }
        X_unsigned = new_X;
        operations_add++;
    }
	operations_add++;
    X_unsigned = original_X;
    operations_sub = 0;
    while (1) {
        unsigned char new_X = X_unsigned - Y_unsigned;
        if (new_X > X_unsigned) {
            break;
        }
        X_unsigned = new_X;
        operations_sub++;
    }
	operations_sub++;
    printf("X is unsigned, Y is unsigned\n");
    printf("Operations X = X + Y: %d\n", operations_add);
    printf("Operations X = X - Y: %d\n", operations_sub);
    printf("\n");

    return 0;
}
