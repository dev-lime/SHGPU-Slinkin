#include <stdio.h>
#include <limits.h>
 
int main()
{
    char x, y;

    printf("Unsigned x, y:\n");
    scanf("%hhd %hhd", &x, &y);
    unsigned char uxp = x, uxm = x, uy = y;
    
    printf("Signed x, y:\n");
    scanf("%hhd %hhd", &x, &y);
    signed char sxp = x, sxm = x, sy = y;
    
    int ucp, ucm, scp, scm,
		oucp, oucm, oscp, oscm,
		uc_min = 0,
		uc_max = UCHAR_MAX,
		sc_min = SCHAR_MIN,
		sc_max = SCHAR_MAX;

    for (ucp = 0; uxp < uc_max; ucp++)
    {
		uxp+=uy;
	}
	printf("ucp, ");
	ucp++;
	for (ucm = 0; uxm > uc_min; ucm++)
    {
		uxm-=uy;
	}
	printf("ucm, ");
	ucm++;
	for (scp = 0; sxp < sc_max; scp++)
    {
		sxp+=uy;
	}
	printf("scp, ");
	scp++;
	for (scm = 0; sxm > sc_min; scm++)
    {
		sxm-=uy;
	}
	printf("scm.");
	scm++;
    printf("\nUNSIGNED Y:\nUnsign x++: %d\nUnsign x--: %d\nSign x++: %d\nSign x--: %d\n", ucp, ucm, scp, scm);
	
	for (ucp = 0; uxp < uc_max; ucp++)
    {
		uxp+=sy;
	}
	ucp++;
	for (ucm = 0; uxm > uc_min; ucm++)
    {
		uxm-=sy;
	}
	ucm++;
	for (scp = 0; sxp < sc_max; scp++)
    {
		sxp+=sy;
	}
	scp++;
	for (scm = 0; sxm > sc_min; scm++)
    {
		sxm-=sy;
	}
	scm++;
    printf("\nSIGNED Y:\nUnsign x++: %d\nUnsign x--: %d\nSign x++: %d\nSign x--: %d\n", ucp, ucm, scp, scm);
    
    return 0;
}
