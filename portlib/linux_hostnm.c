/* Linux hostnm routine */
 
#include <unistd.h>
 
/* function names link differently depending on OS */
 
#include "fpreproc/f77name.h"
 
#if defined __IBM
int F77NAME(hostnm_dummy)(name,len)
#elif defined __LINUX || __OSX
int F77NAME(hostnm)(name,len)
#else
int F77NAME(hostnm_dummy)(name,len)
#endif
char *name;
int len;
{
      return gethostname(name,len);
}


#if defined __IBM
int F77NAME(hostnm_dummy_)(name,len)
#elif defined __LINUX || __OSX
int F77NAME(hostnm_)(name,len)
#else
int F77NAME(hostnm_dummy_)(name,len)
#endif
char *name; int len;
{
	return( gethostname(name,len) );
}


