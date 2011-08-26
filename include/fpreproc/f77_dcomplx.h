#ifndef _f77_dcomplx      /* cpp code for rename of double complex */
#define _f77_dcomplx      /* fortran intrinsic functions */
 
#ifndef __SINGLE_PRECISION
#ifndef __DOUBLE_PRECISION
 
/*  if neither __SINGLE_PRECISION nor __DOUBLE_PRECISION are defined, */
/*  assume __DOUBLE_PRECISION  unless __CRAY is set.  */
 
#ifdef __CRAY
#define __SINGLE_PRECISION
#else
#define __DOUBLE_PRECISION
#endif
 
#endif  /* ifndef DOUBLE_PRECISION */
#endif  /* ifndef SINGLE_PRECISION */
 
#ifdef __DOUBLE_PRECISION       /* workstation f77 -- double precision */
 
#define AREAL DBLE
#define AIMAG DIMAG
#define CMPLX DCMPLX
 
#else                         /* CRAY f77 source -- single precision */
 
#define AREAL REAL
#undef AIMAG
#undef CMPLX
 
#endif  /* DOUBLE or SINGLE_PRECISION */
 
#endif  /* ifndef f77_dcomplx */
