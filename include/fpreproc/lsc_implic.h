#define __DOUBLE
! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     File: Implic.inc
#ifdef __DOUBLE
#define REAL real*8
#define AREAL real
#define AIMAG DIMAG
#define CMPLX DCMPLX
#ifndef __G77
#define COMPLEX complex*16
#endif
#else
#define AREAL real
#endif
      IMPLICIT NONE

