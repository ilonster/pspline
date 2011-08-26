#ifndef _LINPACK__
#define _LINPACK__
/* single <-> double precision name mapping for LINPACK (netlib) routines */
/* D. McCune 21 July 1999       */
 
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
 
/* single real -> double real */
 
#define R1MACH D1MACH
 
#define SCHDC  DCHDC
#define SCHDD  DCHDD
#define SCHEX  DCHEX
#define SCHUD  DCHUD
#define SGBCO  DGBCO
#define SGBDI  DGBDI
#define SGBFA  DGBFA
#define SGBSL  DGBSL
#define SGECO  DGECO
#define SGEDI  DGEDI
#define SGEFA  DGEFA
#define SGESL  DGESL
#define SGTSL  DGTSL
#define SPBCO  DPBCO
#define SPBDI  DPBDI
#define SPBFA  DPBFA
#define SPBSL  DPBSL
#define SPOCO  DPOCO
#define SPODI  DPODI
#define SPOFA  DPOFA
#define SPOSL  DPOSL
#define SPPCO  DPPCO
#define SPPDI  DPPDI
#define SPPFA  DPPFA
#define SPPSL  DPPSL
#define SPTSL  DPTSL
#define SQRDC  DQRDC
#define SQRSL  DQRSL
#define SSICO  DSICO
#define SSIDI  DSIDI
#define SSIFA  DSIFA
#define SSISL  DSISL
#define SSPCO  DSPCO
#define SSPDI  DSPDI
#define SSPFA  DSPFA
#define SSPSL  DSPSL
#define SSVDC  DSVDC
#define STRCO  DTRCO
#define STRDI  DTRDI
#define STRSL  DTRSL
 
/* single complex -> double complex */
 
#define CCHDC  ZCHDC
#define CCHDD  ZCHDD
#define CCHEX  ZCHEX
#define CCHUD  ZCHUD
#define CGBCO  ZGBCO
#define CGBDI  ZGBDI
#define CGBFA  ZGBFA
#define CGBSL  ZGBSL
#define CGECO  ZGECO
#define CGEDI  ZGEDI
#define CGEFA  ZGEFA
#define CGESL  ZGESL
#define CGTSL  ZGTSL
#define CPBCO  ZPBCO
#define CPBDI  ZPBDI
#define CPBFA  ZPBFA
#define CPBSL  ZPBSL
#define CPOCO  ZPOCO
#define CPODI  ZPODI
#define CPOFA  ZPOFA
#define CPOSL  ZPOSL
#define CPPCO  ZPPCO
#define CPPDI  ZPPDI
#define CPPFA  ZPPFA
#define CPPSL  ZPPSL
#define CPTSL  ZPTSL
#define CQRDC  ZQRDC
#define CQRSL  ZQRSL
#define CSICO  ZSICO
#define CSIDI  ZSIDI
#define CSIFA  ZSIFA
#define CSISL  ZSISL
#define CSPCO  ZSPCO
#define CSPDI  ZSPDI
#define CSPFA  ZSPFA
#define CSPSL  ZSPSL
#define CSVDC  ZSVDC
#define CTRCO  ZTRCO
#define CTRDI  ZTRDI
#define CTRSL  ZTRSL
 
#define CHICO  ZHICO
#define CHIDI  ZHIDI
#define CHIFA  ZHIFA
#define CHISL  ZHISL
#define CHPCO  ZHPCO
#define CHPDI  ZHPDI
#define CHPFA  ZHPFA
#define CHPSL  ZHPSL
 
#endif /* DOUBLE */
 
#ifdef __SINGLE_PRECISION
 
/* double real -> single real */
 
#define D1MACH R1MACH
 
#define DCHDC  SCHDC
#define DCHDD  SCHDD
#define DCHEX  SCHEX
#define DCHUD  SCHUD
#define DGBCO  SGBCO
#define DGBDI  SGBDI
#define DGBFA  SGBFA
#define DGBSL  SGBSL
#define DGECO  SGECO
#define DGEDI  SGEDI
#define DGEFA  SGEFA
#define DGESL  SGESL
#define DGTSL  SGTSL
#define DPBCO  SPBCO
#define DPBDI  SPBDI
#define DPBFA  SPBFA
#define DPBSL  SPBSL
#define DPOCO  SPOCO
#define DPODI  SPODI
#define DPOFA  SPOFA
#define DPOSL  SPOSL
#define DPPCO  SPPCO
#define DPPDI  SPPDI
#define DPPFA  SPPFA
#define DPPSL  SPPSL
#define DPTSL  SPTSL
#define DQRDC  SQRDC
#define DQRSL  SQRSL
#define DSICO  SSICO
#define DSIDI  SSIDI
#define DSIFA  SSIFA
#define DSISL  SSISL
#define DSPCO  SSPCO
#define DSPDI  SSPDI
#define DSPFA  SSPFA
#define DSPSL  SSPSL
#define DSVDC  SSVDC
#define DTRCO  STRCO
#define DTRDI  STRDI
#define DTRSL  STRSL
 
 
/* double complex -> single complex */
 
#define ZCHDC  CCHDC
#define ZCHDD  CCHDD
#define ZCHEX  CCHEX
#define ZCHUD  CCHUD
#define ZGBCO  CGBCO
#define ZGBDI  CGBDI
#define ZGBFA  CGBFA
#define ZGBSL  CGBSL
#define ZGECO  CGECO
#define ZGEDI  CGEDI
#define ZGEFA  CGEFA
#define ZGESL  CGESL
#define ZGTSL  CGTSL
#define ZPBCO  CPBCO
#define ZPBDI  CPBDI
#define ZPBFA  CPBFA
#define ZPBSL  CPBSL
#define ZPOCO  CPOCO
#define ZPODI  CPODI
#define ZPOFA  CPOFA
#define ZPOSL  CPOSL
#define ZPPCO  CPPCO
#define ZPPDI  CPPDI
#define ZPPFA  CPPFA
#define ZPPSL  CPPSL
#define ZPTSL  CPTSL
#define ZQRDC  CQRDC
#define ZQRSL  CQRSL
#define ZSICO  CSICO
#define ZSIDI  CSIDI
#define ZSIFA  CSIFA
#define ZSISL  CSISL
#define ZSPCO  CSPCO
#define ZSPDI  CSPDI
#define ZSPFA  CSPFA
#define ZSPSL  CSPSL
#define ZSVDC  CSVDC
#define ZTRCO  CTRCO
#define ZTRDI  CTRDI
#define ZTRSL  CTRSL
 
#define ZHICO  CHICO
#define ZHIDI  CHIDI
#define ZHIFA  CHIFA
#define ZHISL  CHISL
#define ZHPCO  CHPCO
#define ZHPDI  CHPDI
#define ZHPFA  CHPFA
#define ZHPSL  CHPSL
 
#endif /* SINGLE */
 
#endif /* _LINPACK__ */
