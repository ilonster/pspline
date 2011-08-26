 /* C source for I1MACH */
 /* Note that some values may need changing -- see the comments below. */
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <math.h>
 
#include "fpreproc/f77name.h"   /* fortran linkage for i1mach, r1mach, d1mach */
 
 /* C source for R1MACH */
 
float F77NAME(r1mach)(int *i)
{
       switch(*i){
         case 1: return FLT_MIN;
         case 2: return FLT_MAX;
         case 3: return FLT_EPSILON/FLT_RADIX;
         case 4: return FLT_EPSILON;
         case 5: return (float)log10((double)FLT_RADIX);
         }
 
       fprintf(stderr, "invalid argument: r1mach(%ld)\n", *i);
       exit(EXIT_FAILURE);
       return 0; /* for compilers that complain of missing return values */
       }
