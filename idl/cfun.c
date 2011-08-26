#include <stdio.h>

float cfun(float x)
{
  return 2.0*x;
}

float cfun2(int argc, void *argv[] )
{
    float x,ans;

    x =   *( (float *) argv[0] ) ;
    ans = 2.0*x*x; 

    return  ans;
}
