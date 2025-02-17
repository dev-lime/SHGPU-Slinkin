#include <stdio.h>
#include <limits.h>
 
int main()
{
	// 4 ввода вместо двух, два для signed (x,y) два для unsign (x,y)
    char x, y;

    printf("Unsigned x, y:\n");
    scanf("%hhd %hhd", &x, &y);
    unsigned char uxp = x, uxm = x, uy = y;
    
    printf("Signed x, y:\n");
    scanf("%hhd %hhd", &x, &y);
    signed char sxp = x, sxm = x, sy = y;
    
    int ucp, ucm, scp, scm;
    
    for (ucp = 0; uxp < UCHAR_MAX; ucp++)
    {
		uxp+=uy;
	}
	ucp++;
	for (ucm = 0; uxm > 0; ucm++)
    {
		uxm-=uy;
	}
	ucm++;
	for (scp = 0; sxp < SCHAR_MAX; scp++)
    {
		sxp+=uy;
	}
	scp++;
	for (scm = 0; sxm > SCHAR_MIN; scm++)
    {
		sxm-=uy;
	}
	scm++;
    printf("\nUNSIGNED Y:\nUnsign x++: %d\nUnsign x--: %d\nSign x++: %d\nSign x--: %d\n", ucp, ucm, scp, scm);
	
	for (ucp = 0; uxp < UCHAR_MAX; ucp++)
    {
		uxp+=sy;
	}
	ucp++;
	for (ucm = 0; uxm > 0; ucm++)
    {
		uxm-=sy;
	}
	ucm++;
	for (scp = 0; sxp < SCHAR_MAX; scp++)
    {
		sxp+=sy;
	}
	scp++;
	for (scm = 0; sxm > SCHAR_MIN; scm++)
    {
		sxm-=sy;
	}
	scm++;
    printf("\nSIGNED Y:\nUnsign x++: %d\nUnsign x--: %d\nSign x++: %d\nSign x--: %d\n", ucp, ucm, scp, scm);
    
    return 0;
}
