/* config.h.  Generated automatically by configure.  */
/*   (dmc for TRANSP):  __CRAY test added; configure not used.   */
#define F90_WORKS 1
#define REALOK 1
#define ARITHOK 1
#define USEINT 0
/* #undef HIPREC */

#if !ARITHOK
# ERROR
#endif
#if !REALOK
# ERROR
#endif
#if !F90_WORKS
# ERROR
#endif

#ifdef __CRAY
#define HIPREC 1
#else
#define HIPREC 0
#endif

#if HIPREC
#define REAL8 real
#define ONE 1.0e0
#define TWO 2.0e0
#define __PI 3.14159265358979323846264338328e0
#else
#define REAL8 real*8
#define ONE 1.0d0
#define TWO 2.0d0
#define __PI 3.14159265358979323846264338328d0
#endif

#define __RPI 3.14159265358979323846264338328
#define __SPI 3.14159265358979323846264338328_single
#define __DPI 3.14159265358979323846264338328_double
