/*
Дано однобайтовое значение X. Создать программу, определяющую сколько потребуется последовательных операций инкремента и декримента для перехода через границу типа переменной X. Рассмотреть варианты типов signed char и unsigned char для переменной X.
*/

#include <stdio.h>
#include <limits.h>

int main()
{
    char x;

    printf("Unsigned: ");
    scanf("%hhd", &x);

    // Преобразует x в unsigned char для инкремента и декремента
    unsigned char uxp = x, uxm = x;
    printf("UXP = %hhu\n", uxp);

    printf("Signed: ");
    scanf("%hhd", &x);

    // Преобразует x в signed char для инкремента и декремента
    signed char sxp = x, sxm = x;
    printf("SXP = %hhd\n\n", sxp);

    int ucp, ucm, scp, scm;

    // Считает количество инкрементов до переполнения unsigned char
    for (ucp = 0; uxp < UCHAR_MAX; ucp++)
    {
        uxp++;
    }
    ucp++; // Выполняет переполнение

    // Считает количество декрементов до переполнения unsigned char
    for (ucm = 0; uxm > 0; ucm++)
    {
        uxm--;
    }
    ucm++; // Выполняет переполнение

    // Считает количество инкрементов до переполнения signed char
    for (scp = 0; sxp < SCHAR_MAX; scp++)
    {
        sxp++;
    }
    scp++; // Выполняет переполнение

    // Считает количество декрементов до переполнения signed char
    for (scm = 0; sxm > SCHAR_MIN; scm++)
    {
        sxm--;
    }
    scm++; // Выполняет переполнение

    printf("Unsign++: %d\nUnsign--: %d\nSign++: %d\nSign--: %d\n", ucp, ucm, scp, scm);

    return 0;
}
