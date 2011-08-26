#include "fpreproc/f77name.h"
 
/*  find the max record length of an ascii file, defined as follows:  */
/*    max record length = max no. of bytes between ascii CR or LF     */
 
#include <stdio.h>
 
#define ASCII_CR  13
#define ASCII_LF  10

 
int F77NAME(max_crec)(const char* filename)
{
  FILE *f;
  int imax,icount,ics;
  int ch;
#ifdef __CRAY
  const char *string;
  int  lstr;
#endif
 
  /*  open file */
 
#ifdef __CRAY
  lstr = strlen(filename);
  string = (const char *)malloc(lstr+1);
  strcpy((char*)string,(char*)filename);
  f = fopen(string,"r");
  free((void *)string);
#else
  f = fopen(filename,"r");
#endif
  if ( f == NULL ) return(-1);       /* error opening file */
 
  /*  scan file */
 
  imax = 0;
  icount = 0;
 
  while((ch=getc(f)) >= 0 )
    {
      if ( ch == ASCII_CR || ch == ASCII_LF )
	{
	  if ( icount > imax) imax = icount;
	  icount = 0;
	}
      else
	{
	  icount++;
	}
    }
 
  ics = fclose(f);
 
  return(imax);
 
}

int F77NAME(max_crec_)(const char* filename)
{
	return( F77NAME(max_crec)(filename) );
}
