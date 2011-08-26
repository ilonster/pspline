/*  f77 callable routine to get cwd for Cray X1 only */
 
#if  __X1
 
#include "fpreproc/f77name.h"
#include <unistd.h>


 
int F77NAME(cgetcwd)(var,val,var_len,val_len)
     char *var,*val;
     int var_len,val_len;
{

  char *var_a,*val_a;
  int i;
 
  var_a = (char *)malloc(var_len + 1);
  strncpy(var_a,var,var_len);
  var_a[var_len] = '\0';
 
  val_a = getcwd(var_a, var_len);
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
void cgetcwd_dummy() {}
#endif
