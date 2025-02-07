#include <stdio.h>
#include <limits.h>
 
int main()
{
    char x, y;
    scanf("%hhd %hhd", &x, &y);
    unsigned char uxp = x, uxm = x, uy = y;
    signed char sxp = x, sxm = x, sy = y;
    int ucp, ucm, scp, scm;
    //
    printf("%d, %d", uy, sy);
    //
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
	
    printf("UNSIGNED Y:\nUnsign++: %d\nUnsign--: %d\nSign++: %d\nSign--: %d\n", ucp, ucm, scp, scm);
	
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
    
    printf("\nSIGNED Y:\nUnsign++: %d\nUnsign--: %d\nSign++: %d\nSign--: %d\n", ucp, ucm, scp, scm);
    
    return 0;
}
