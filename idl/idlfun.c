#include <stdio.h>

float funfun(float *x)
{
  return *x;
}

float idlfun(int argc, void *argv[] )
{
    float x,ans;

    x =   *( (float *) argv[0] ) ;
    ans = 2.0*x*x; 

    return  ans;
}

