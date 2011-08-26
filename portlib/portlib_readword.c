#include <errno.h>
#include <stdio.h>
#include <string.h>
 
#include "fpreproc/f77name.h"


 
void F77NAME(portlib_readword)(fname,cword,imaxlen,ier)
     const char *fname;   /* name of file */
     char *cword;         /* word read out of file (returned) */
     int *imaxlen;        /* max length of word, *including* trailing null */
     int *ier;            /* returns 0 if OK, errno value if error on OPEN */
{

  /*  read first word in a file -- terminated by blank, null, */
  /*  or non-printable character, or EOF */

#ifdef __CRAY
  const char *string;
  int  lstr;
#endif
 
#define IO_ERRTEST errno != 0
 
#ifdef __SGI
 #undef IO_ERRTEST
 #define IO_ERRTEST errno != 0 && errno != 22
#endif
 
  FILE *f;
  int  istat;
  int j,c;
 
/* executable code */
 
  errno = 0;
 
#ifdef __CRAY
  lstr = strlen(fname);
  string = (const char *)malloc(lstr+1);
  strcpy((char*)string,(char*)fname);
  f = fopen(string,"rb");
  free((void *)string);
#else
  f = fopen(fname, "r");
#endif
 
  if ( f == NULL )
    {
      *ier = errno;
      printf("readword: file open error: %s\n",fname);
      if ( errno != 0 )
	{
	  perror(fname);
	}
      else
	{
	  *ier=1;
	}
      return;
    }
 
  j = 0;
  while ( j < *imaxlen )
    {
      cword[j]=0;
      j++;
    }
 
  errno = 0;
 
  j = 0;
  while ( j < *imaxlen )
    {
      c = getc( f );
      if ( IO_ERRTEST )
	{
	  j = *imaxlen;
	}
      else
	{
	  if ( c > 32 && c < 127 )
	    {
	      cword[j] = c;
	    }
	  else
	    {
	      j = *imaxlen;
	    }
	}
      j++;
    }
 
  *ier = 0;
 
  errno = 0;
  istat = fclose ( f );
  return;
}

void F77NAME(portlib_readword_)(fname,cword,imaxlen,ier)
     const char *fname;   /* name of file */
     char *cword;         /* word read out of file (returned) */
     int *imaxlen;        /* max length of word, *including* trailing null */
     int *ier;            /* returns 0 if OK, errno value if error on OPEN */
{
	F77NAME(portlib_readword)(fname,cword,imaxlen,ier);
}
