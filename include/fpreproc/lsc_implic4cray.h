! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     File: Implic.inc
#undef REAL
#ifdef REAL
#define COMPLEX complex*16
#define AREAL real
#define AIMAG DIMAG
#define CMPLX DCMPLX
#else
#define AREAL real
#undef  AIMAG
#undef  CMPLX 
#endif
      IMPLICIT NONE
