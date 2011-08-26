#include <stdlib.h>
#include "fpreproc/f77name.h"
 
/* fortran access to C (unix) library "system" call */
/* fortran must pass a null terminated byte string  */
 
int F77NAME(fsystem)(const char* cmd)
{
  return system(cmd);
}
