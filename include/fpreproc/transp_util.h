#ifndef TRANSP_UTIL_H
#define TRANSP_UTIL_H
//
// -------------------- typedefs ----------------------------
//

typedef int ip_32 ;
typedef int lp_32 ;  // logical*4

const lp_32 lp_32_TRUE  = -1 ; // logical*4=.TRUE.
const lp_32 lp_32_FALSE = 0 ;  // logical*4=.FALSE.

#if __CRAY
typedef float  fp_64;
typedef float  fp_32;
#else
typedef double fp_64;
typedef float  fp_32;
#endif

#endif
