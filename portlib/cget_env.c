/*  f77 callable routine to get environment variable -- for the IBM only */

#include <stdlib.h>
#include <string.h>
 
#if __IBM || __RS6000 || __IDL6_FIX || __X1
 
#include "fpreproc/f77name.h"
 
int F77NAME(cget_env)(var,val,var_len,val_len)
     char *var,*val;
     int var_len,val_len;
{
  char *var_a,*val_a;
  int i;
 
  var_a = (char *)malloc(var_len + 1);
  strncpy(var_a,var,var_len);
  var_a[var_len] = '\0';
 
  val_a = (char *)getenv(var_a);
  free(var_a);
 
  if (val_a == (char *)0 )
    return 0;
  else {
    strncpy(val,val_a,val_len);
    F77NAME(str_pad)(val,val_len);
    return 1;
  }
}
#else
void cget_env_dummy() {}
#endif
