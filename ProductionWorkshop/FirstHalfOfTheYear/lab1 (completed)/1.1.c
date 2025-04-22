#include <stdio.h>
#include <limits.h>
 
int main()
{
    char x;
    printf("Unsigned: ");
    scanf("%hhd", &x);
    unsigned char uxp = x, uxm = x;
    printf("UXP = %hhu\n", uxp); // x=[0; 255]; 256 - x; 1 + x
    
    printf("Signed: ");
    scanf("%hhd", &x);
    signed char sxp = x, sxm = x;
    printf("SXP = %hhd\n\n", sxp); // x=[-127; 127]; 128 - x; 129 + x
    
    int ucp, ucm, scp, scm;
    for (ucp = 0; uxp < UCHAR_MAX; ucp++)
	{
		uxp++;
	}
	ucp++;
	for (ucm = 0; uxm > 0; ucm++)
    {
		uxm--;
	}
	ucm++;
	for (scp = 0; sxp < SCHAR_MAX; scp++)
    {
		sxp++;
	}
	scp++;
	for (scm = 0; sxm > SCHAR_MIN; scm++)
    {
		sxm--;
	}
	scm++;
    
    printf("Unsign++: %d\nUnsign--: %d\nSign++: %d\nSign--: %d\n", ucp, ucm, scp, scm);
    
    return 0;
}
