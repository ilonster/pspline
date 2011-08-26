/* c_abort.c */
/* abnormal exit callable from fortran */
 
#include <stdlib.h>
 
/* function names link differently depending on OS */
 
#include "fpreproc/f77name.h"
 
void F77NAME(c_abort )()
{
  printf(" ...portlib/c_abort.c called.\n");
  abort();
}
void F77NAME(c_abort_)()
{ F77NAME(c_abort )(); }

