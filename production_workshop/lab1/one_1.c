#include <stdio.h>
#include <limits.h>
 
int main()
{
    char x;
    scanf("%hhd", &x);
    unsigned char uxp = x, uxm = x;
    signed char sxp = x, sxm = x;
    int ucp, ucm, scp, scm;
    printf("%hhu\n", uxp);
    printf("%hhd\n", sxp);
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
