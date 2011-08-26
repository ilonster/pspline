#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "export.h" /*IDL export library for IDL typedefs, etc. */

#if UNDERSCORE 
  #define PSPLINE pspline_
  #define TEST test_
#else
  #define PSPLINE pspline
  #define TEST test
#endif

#define FORT(func) ( UNDERSCORE ? func'_' : func )

float ftest(int argc, void *argv[])
{
  float x, ans;
  extern float TEST(float*);

/*  printf("ftest\n");*/
  x = *(float *) argv[0];

  ans = TEST(&x);
  ans = 2.0*TEST((float*) argv[0]);
/*  printf("%e\n",ans);*/

  return ans;
}

int pspline_idl(int argc, void *argv[])
{
/*
      SUBROUTINE PSPLINE (N, X, Y, B, C, D, WK)
      INTEGER N
      REAL X(N), Y(N), B(N), C(N), D(N), WK(N)
*/
    printf("%s\n", FORT(pspline))

    if(argc != 7) {
        printf("pspline_idl: requires 7 arguments");
        return 1;
    }
    PSPLINE( (int*) argv[0], (float*) argv[1], (float*) argv[2], (float*) argv[3], (float*) argv[4],(float*) argv[5],(float*) argv[6],(float*) argv[7]);

    return 0;
}

