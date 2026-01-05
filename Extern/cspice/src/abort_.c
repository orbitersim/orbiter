/*
   06-FEB-1999 (NJB)
	
	The statement
	
      return 0;
		
	for the normal C case was added to suppress compilation warnings.
		
*/

#include "stdio.h"
#include "f2c.h"

#ifdef KR_headers
extern VOID sig_die();

int abort_()
#else
extern void sig_die(char*,int);

int abort_(void)
#endif
{
sig_die("Fortran abort routine called", 1);

return 0;

#ifdef __cplusplus
return 0;
#endif
}
