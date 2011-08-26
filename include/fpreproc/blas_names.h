#ifndef _BLAS__ /* avoid multiple includes */
#define _BLAS__

#ifndef __DOUBLE_PRECISION
#ifndef __SINGLE_PRECISION
#ifdef __CRAY
#define __SINGLE_PRECISION
#else
#define __DOUBLE_PRECISION
#endif
#endif
#endif

#ifdef __DOUBLE_PRECISION

#define R1MACH D1MACH

#define CAXPY ZAXPY
#define CCOPY ZCOPY
#define CDOTC ZDOTC
#define CDOTU ZDOTU
#define CGBMV ZGBMV
#define CGEMM ZGEMM
#define CGEMV ZGEMV
#define CGERC ZGERC
#define CGERU ZGERU
#define CHBMV ZHBMV
#define CHEMM ZHEMM
#define CHEMV ZHEMV
#define CHER ZHER
#define CHER2 ZHER2
#define CHER2K ZHER2K
#define CHERK ZHERK
#define CHPMV ZHPMV
#define CHPR ZHPR
#define CHPR2 ZHPR2
#define CROTG ZROTG
#define CSCAL ZSCAL
#define CSROT ZDROT
#define CSSCAL ZDSCAL
#define CSWAP ZSWAP
#define CSYMM ZSYMM
#define CSYR2K ZSYR2K
#define CSYRK ZSYRK
#define CTBMV ZTBMV
#define CTBSV ZTBSV
#define CTPMV ZTPMV
#define CTPSV ZTPSV
#define CTRMM ZTRMM
#define CTRMV ZTRMV
#define CTRSM ZTRSM
#define CTRSV ZTRSV
#define ICAMAX IZAMAX
#define ISAMAX IDAMAX
#define SASUM DASUM
#define SAXPY DAXPY
#define SCASUM DZASUM
#define SCNRM2 DZNRM2
#define SCOPY DCOPY
#define SDOT DDOT
#define SGBMV DGBMV
#define SGEMM DGEMM
#define SGEMV DGEMV
#define SGER DGER
#define SNRM2 DNRM2
#define SROT DROT
#define SROTG DROTG
#define SROTM DROTM
#define SROTMG DROTMG
#define SSBMV DSBMV
#define SSCAL DSCAL
#define SSPMV DSPMV
#define SSPR DSPR
#define SSPR2 DSPR2
#define SSWAP DSWAP
#define SSYMM DSYMM
#define SSYMV DSYMV
#define SSYR DSYR
#define SSYR2 DSYR2
#define SSYR2K DSYR2K
#define SSYRK DSYRK
#define STBMV DTBMV
#define STBSV DTBSV
#define STPMV DTPMV
#define STPSV DTPSV
#define STRMM DTRMM
#define STRMV DTRMV
#define STRSM DTRSM
#define STRSV DTRSV
#endif
 
#ifdef __SINGLE_PRECISION

#define D1MACH R1MACH

#define ZAXPY CAXPY
#define ZCOPY CCOPY
#define ZDOTC CDOTC
#define ZDOTU CDOTU
#define ZGBMV CGBMV
#define ZGEMM CGEMM
#define ZGEMV CGEMV
#define ZGERC CGERC
#define ZGERU CGERU
#define ZHBMV CHBMV
#define ZHEMM CHEMM
#define ZHEMV CHEMV
#define ZHER CHER
#define ZHER2 CHER2
#define ZHER2K CHER2K
#define ZHERK CHERK
#define ZHPMV CHPMV
#define ZHPR CHPR
#define ZHPR2 CHPR2
#define ZROTG CROTG
#define ZSCAL CSCAL
#define ZDROT CSROT
#define ZDSCAL CSSCAL
#define ZSWAP CSWAP
#define ZSYMM CSYMM
#define ZSYR2K CSYR2K
#define ZSYRK CSYRK
#define ZTBMV CTBMV
#define ZTBSV CTBSV
#define ZTPMV CTPMV
#define ZTPSV CTPSV
#define ZTRMM CTRMM
#define ZTRMV CTRMV
#define ZTRSM CTRSM
#define ZTRSV CTRSV
#define IZAMAX ICAMAX
#define IDAMAX ISAMAX
#define DASUM SASUM
#define DAXPY SAXPY
#define DZASUM SCASUM
#define DZNRM2 SCNRM2
#define DCOPY SCOPY
#define DDOT SDOT
#define DGBMV SGBMV
#define DGEMM SGEMM
#define DGEMV SGEMV
#define DGER SGER
#define DNRM2 SNRM2
#define DROT SROT
#define DROTG SROTG
#define DROTM SROTM
#define DROTMG SROTMG
#define DSBMV SSBMV
#define DSCAL SSCAL
#define DSPMV SSPMV
#define DSPR SSPR
#define DSPR2 SSPR2
#define DSWAP SSWAP
#define DSYMM SSYMM
#define DSYMV SSYMV
#define DSYR SSYR
#define DSYR2 SSYR2
#define DSYR2K SSYR2K
#define DSYRK SSYRK
#define DTBMV STBMV
#define DTBSV STBSV
#define DTPMV STPMV
#define DTPSV STPSV
#define DTRMM STRMM
#define DTRMV STRMV
#define DTRSM STRSM
#define DTRSV STRSV
#endif

#endif /* _BLAS__ */
