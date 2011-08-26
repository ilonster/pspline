#ifndef _LAPACK__
#define _LAPACK__
/* single <-> double precision name mapping */
/* A. Pletzer Mar 29 1999       */
/* removed lower case mapping May 4 99 (ap) */
 
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
 
#define SBDSQR DBDSQR
#define SDISNA DDISNA
#define SGBBRD DGBBRD
#define SGBCON DGBCON
#define SGBEQU DGBEQU
#define SGBRFS DGBRFS
#define SGBSVX DGBSVX
#define SGBTRF DGBTRF
#define SGBTRS DGBTRS
#define SGEBAK DGEBAK
#define SGEBAL DGEBAL
#define SGEBRD DGEBRD
#define SGECON DGECON
#define SGEEQU DGEEQU
#define SGEES DGEES
#define SGEESX DGEESX
#define SGEEV DGEEV
#define SGEEVX DGEEVX
#define SGEGS DGEGS
#define SGEGV DGEGV
#define SGEHRD DGEHRD
#define SGELQF DGELQF
#define SGELS DGELS
#define SGELSS DGELSS
#define SGELSX DGELSX
#define SGEQLF DGEQLF
#define SGEQPF DGEQPF
#define SGEQRF DGEQRF
#define SGERFS DGERFS
#define SGERQF DGERQF
#define SGESV DGESV
#define SGESVD DGESVD
#define SGESVX DGESVX
#define SGETRF DGETRF
#define SGETRI DGETRI
#define SGETRS DGETRS
#define SGGBAK DGGBAK
#define SGGBAL DGGBAL
#define SGGGLM DGGGLM
#define SGGHRD DGGHRD
#define SGGQRF DGGQRF
#define SGGRQF DGGRQF
#define SGGSVD DGGSVD
#define SGGSVP DGGSVP
#define SGTCON DGTCON
#define SGTRFS DGTRFS
#define SGTTRF DGTTRF
#define SGTTRS DGTTRS
#define SHGEQZ DHGEQZ
#define SHSEIN DHSEIN
#define SHSEQR DHSEQR
#define SLAMCH DLAMCH
#define SOPGTR DOPGTR
#define SOPMTR DOPMTR
#define SORGBR DORGBR
#define SORGHR DORGHR
#define SORGLQ DORGLQ
#define SORGQR DORGQR
#define SORGTR DORGTR
#define SORMBR DORMBR
#define SORMHR DORMHR
#define SORMLQ DORMLQ
#define SORMQR DORMQR
#define SORMRQ DORMRQ
#define SORMTR DORMTR
#define SPBCON DPBCON
#define SPBEQU DPBEQU
#define SPBRFS DPBRFS
#define SPBSTF DPBSTF
#define SPBTRF DPBTRF
#define SPBTRS DPBTRS
#define SPOCON DPOCON
#define SPOEQU DPOEQU
#define SPORFS DPORFS
#define SPOTRF DPOTRF
#define SPOTRI DPOTRI
#define SPOTRS DPOTRS
#define SPPCON DPPCON
#define SPPEQU DPPEQU
#define SPPRFS DPPRFS
#define SPPTRF DPPTRF
#define SPPTRI DPPTRI
#define SPPTRS DPPTRS
#define SPTCON DPTCON
#define SPTEQR DPTEQR
#define SPTRFS DPTRFS
#define SPTTRF DPTTRF
#define SPTTRS DPTTRS
#define SSBEV DSBEV
#define SSBEVD DSBEVD
#define SSBEVX DSBEVX
#define SSBGST DSBGST
#define SSBGV DSBGV
#define SSBTRD DSBTRD
#define SSPCON DSPCON
#define SSPEV DSPEV
#define SSPEVD DSPEVD
#define SSPEVX DSPEVX
#define SSPGV DSPGV
#define SSPRFS DSPRFS
#define SSPTRD DSPTRD
#define SSPTRF DSPTRF
#define SSPTRI DSPTRI
#define SSPTRS DSPTRS
#define SSTEBZ DSTEBZ
#define SSTEDC DSTEDC
#define SSTEIN DSTEIN
#define SSTEQR DSTEQR
#define SSTERF DSTERF
#define SSTEV DSTEV
#define SSTEVD DSTEVD
#define SSTEVX DSTEVX
#define SSYCON DSYCON
#define SSYEV DSYEV
#define SSYEVD DSYEVD
#define SSYEVX DSYEVX
#define SSYGV DSYGV
#define SSYRFS DSYRFS
#define SSYTRD DSYTRD
#define SSYTRF DSYTRF
#define SSYTRI DSYTRI
#define SSYTRS DSYTRS
#define STBCON DTBCON
#define STBRFS DTBRFS
#define STBTRS DTBTRS
#define STGEVC DTGEVC
#define STGSJA DTGSJA
#define STPCON DTPCON
#define STPRFS DTPRFS
#define STPTRI DTPTRI
#define STPTRS DTPTRS
#define STRCON DTRCON
#define STREVC DTREVC
#define STREXC DTREXC
#define STRRFS DTRRFS
#define STRSEN DTRSEN
#define STRSNA DTRSNA
#define STRSYL DTRSYL
#define STRTRI DTRTRI
#define STRTRS DTRTRS
#define STZRQF DTZRQF
 
/* single complex -> double complex */
 
#define ICMAX1 IZMAX1
#define CBDSQR ZBDSQR
#define CGBBRD ZGBBRD
#define CGBCON ZGBCON
#define CGBEQU ZGBEQU
#define CGBRFS ZGBRFS
#define CGBSVX ZGBSVX
#define CGBTRF ZGBTRF
#define CGBTRS ZGBTRS
#define CGEBAK ZGEBAK
#define CGEBAL ZGEBAL
#define CGEBRD ZGEBRD
#define CGECON ZGECON
#define CGEEQU ZGEEQU
#define CGEES ZGEES
#define CGEESX ZGEESX
#define CGEEV ZGEEV
#define CGEEVX ZGEEVX
#define CGEGS ZGEGS
#define CGEGV ZGEGV
#define CGEHRD ZGEHRD
#define CGELQF ZGELQF
#define CGELS ZGELS
#define CGELSS ZGELSS
#define CGELSX ZGELSX
#define CGEQLF ZGEQLF
#define CGEQPF ZGEQPF
#define CGEQRF ZGEQRF
#define CGERFS ZGERFS
#define CGERQF ZGERQF
#define CGESVD ZGESVD
#define CGESVX ZGESVX
#define CGETRF ZGETRF
#define CGETRI ZGETRI
#define CGETRS ZGETRS
#define CGGBAK ZGGBAK
#define CGGBAL ZGGBAL
#define CGGGLM ZGGGLM
#define CGGHRD ZGGHRD
#define CGGQRF ZGGQRF
#define CGGRQF ZGGRQF
#define CGGSVD ZGGSVD
#define CGGSVP ZGGSVP
#define CGTCON ZGTCON
#define CGTRFS ZGTRFS
#define CGTTRF ZGTTRF
#define CGTTRS ZGTTRS
#define CHBEV ZHBEV
#define CHBEVD ZHBEVD
#define CHBEVX ZHBEVX
#define CHBGST ZHBGST
#define CHBGV ZHBGV
#define CHBTRD ZHBTRD
#define CHECON ZHECON
#define CHEEV ZHEEV
#define CHEEVD ZHEEVD
#define CHEEVX ZHEEVX
#define CHEGV ZHEGV
#define CHERFS ZHERFS
#define CHETRD ZHETRD
#define CHETRF ZHETRF
#define CHETRI ZHETRI
#define CHETRS ZHETRS
#define CHGEQZ ZHGEQZ
#define CHPCON ZHPCON
#define CHPEV ZHPEV
#define CHPEVD ZHPEVD
#define CHPEVX ZHPEVX
#define CHPGV ZHPGV
#define CHPRFS ZHPRFS
#define CHPTRD ZHPTRD
#define CHPTRF ZHPTRF
#define CHPTRI ZHPTRI
#define CHPTRS ZHPTRS
#define CHSEIN ZHSEIN
#define CHSEQR ZHSEQR
#define CPBCON ZPBCON
#define CPBEQU ZPBEQU
#define CPBRFS ZPBRFS
#define CPBSTF ZPBSTF
#define CPBTRF ZPBTRF
#define CPBTRS ZPBTRS
#define CPOCON ZPOCON
#define CPOEQU ZPOEQU
#define CPORFS ZPORFS
#define CPOTRF ZPOTRF
#define CPOTRI ZPOTRI
#define CPOTRS ZPOTRS
#define CPPCON ZPPCON
#define CPPEQU ZPPEQU
#define CPPRFS ZPPRFS
#define CPPTRF ZPPTRF
#define CPPTRI ZPPTRI
#define CPPTRS ZPPTRS
#define CPTCON ZPTCON
#define CPTEQR ZPTEQR
#define CPTRFS ZPTRFS
#define CPTTRF ZPTTRF
#define CPTTRS ZPTTRS
#define CSPCON ZSPCON
#define CSPRFS ZSPRFS
#define CSPTRF ZSPTRF
#define CSPTRI ZSPTRI
#define CSPTRS ZSPTRS
#define CSTEDC ZSTEDC
#define CSTEIN ZSTEIN
#define CSTEQR ZSTEQR
#define CSYCON ZSYCON
#define CSYRFS ZSYRFS
#define CSYTRF ZSYTRF
#define CSYTRI ZSYTRI
#define CSYTRS ZSYTRS
#define CTBCON ZTBCON
#define CTBRFS ZTBRFS
#define CTBTRS ZTBTRS
#define CTGEVC ZTGEVC
#define CTGSJA ZTGSJA
#define CTPCON ZTPCON
#define CTPRFS ZTPRFS
#define CTPTRI ZTPTRI
#define CTPTRS ZTPTRS
#define CTRCON ZTRCON
#define CTREVC ZTREVC
#define CTREXC ZTREXC
#define CTRRFS ZTRRFS
#define CTRSEN ZTRSEN
#define CTRSNA ZTRSNA
#define CTRSYL ZTRSYL
#define CTRTRI ZTRTRI
#define CTRTRS ZTRTRS
#define CTZRQF ZTZRQF
#define CUNGBR ZUNGBR
#define CUNGHR ZUNGHR
#define CUNGLQ ZUNGLQ
#define CUNGQR ZUNGQR
#define CUNMBR ZUNMBR
#define CUNMHR ZUNMHR
#define CUNMLQ ZUNMLQ
#define CUNMQR ZUNMQR
#define CUNMRQ ZUNMRQ
#define CUNMTR ZUNMTR
#define CUPGTR ZUPGTR
#endif /* DOUBLE */
 
#ifdef __SINGLE_PRECISION
 
/* double real -> single real */
 
#define D1MACH R1MACH
 
#define DBDSQR SBDSQR
#define DDISNA SDISNA
#define DGBBRD SGBBRD
#define DGBCON SGBCON
#define DGBEQU SGBEQU
#define DGBRFS SGBRFS
#define DGBSVX SGBSVX
#define DGBTRF SGBTRF
#define DGBTRS SGBTRS
#define DGEBAK SGEBAK
#define DGEBAL SGEBAL
#define DGEBRD SGEBRD
#define DGECON SGECON
#define DGEEQU SGEEQU
#define DGEES SGEES
#define DGEESX SGEESX
#define DGEEV SGEEV
#define DGEEVX SGEEVX
#define DGEGS SGEGS
#define DGEGV SGEGV
#define DGEHRD SGEHRD
#define DGELQF SGELQF
#define DGELS SGELS
#define DGELSS SGELSS
#define DGELSX SGELSX
#define DGEQLF SGEQLF
#define DGEQPF SGEQPF
#define DGEQRF SGEQRF
#define DGERFS SGERFS
#define DGERQF SGERQF
#define DGESV SGESV
#define DGESVD SGESVD
#define DGESVX SGESVX
#define DGETRF SGETRF
#define DGETRI SGETRI
#define DGETRS SGETRS
#define DGGBAK SGGBAK
#define DGGBAL SGGBAL
#define DGGGLM SGGGLM
#define DGGHRD SGGHRD
#define DGGQRF SGGQRF
#define DGGRQF SGGRQF
#define DGGSVD SGGSVD
#define DGGSVP SGGSVP
#define DGTCON SGTCON
#define DGTRFS SGTRFS
#define DGTTRF SGTTRF
#define DGTTRS SGTTRS
#define DHGEQZ SHGEQZ
#define DHSEIN SHSEIN
#define DHSEQR SHSEQR
#define DLAMCH SLAMCH
#define DOPGTR SOPGTR
#define DOPMTR SOPMTR
#define DORGBR SORGBR
#define DORGHR SORGHR
#define DORGLQ SORGLQ
#define DORGQR SORGQR
#define DORGTR SORGTR
#define DORMBR SORMBR
#define DORMHR SORMHR
#define DORMLQ SORMLQ
#define DORMQR SORMQR
#define DORMRQ SORMRQ
#define DORMTR SORMTR
#define DPBCON SPBCON
#define DPBEQU SPBEQU
#define DPBRFS SPBRFS
#define DPBSTF SPBSTF
#define DPBTRF SPBTRF
#define DPBTRS SPBTRS
#define DPOCON SPOCON
#define DPOEQU SPOEQU
#define DPORFS SPORFS
#define DPOTRF SPOTRF
#define DPOTRI SPOTRI
#define DPOTRS SPOTRS
#define DPPCON SPPCON
#define DPPEQU SPPEQU
#define DPPRFS SPPRFS
#define DPPTRF SPPTRF
#define DPPTRI SPPTRI
#define DPPTRS SPPTRS
#define DPTCON SPTCON
#define DPTEQR SPTEQR
#define DPTRFS SPTRFS
#define DPTTRF SPTTRF
#define DPTTRS SPTTRS
#define DSBEV SSBEV
#define DSBEVD SSBEVD
#define DSBEVX SSBEVX
#define DSBGST SSBGST
#define DSBGV SSBGV
#define DSBTRD SSBTRD
#define DSPCON SSPCON
#define DSPEV SSPEV
#define DSPEVD SSPEVD
#define DSPEVX SSPEVX
#define DSPGV SSPGV
#define DSPRFS SSPRFS
#define DSPTRD SSPTRD
#define DSPTRF SSPTRF
#define DSPTRI SSPTRI
#define DSPTRS SSPTRS
#define DSTEBZ SSTEBZ
#define DSTEDC SSTEDC
#define DSTEIN SSTEIN
#define DSTEQR SSTEQR
#define DSTERF SSTERF
#define DSTEV SSTEV
#define DSTEVD SSTEVD
#define DSTEVX SSTEVX
#define DSYCON SSYCON
#define DSYEV SSYEV
#define DSYEVD SSYEVD
#define DSYEVX SSYEVX
#define DSYGV SSYGV
#define DSYRFS SSYRFS
#define DSYTRD SSYTRD
#define DSYTRF SSYTRF
#define DSYTRI SSYTRI
#define DSYTRS SSYTRS
#define DTBCON STBCON
#define DTBRFS STBRFS
#define DTBTRS STBTRS
#define DTGEVC STGEVC
#define DTGSJA STGSJA
#define DTPCON STPCON
#define DTPRFS STPRFS
#define DTPTRI STPTRI
#define DTPTRS STPTRS
#define DTRCON STRCON
#define DTREVC STREVC
#define DTREXC STREXC
#define DTRRFS STRRFS
#define DTRSEN STRSEN
#define DTRSNA STRSNA
#define DTRSYL STRSYL
#define DTRTRI STRTRI
#define DTRTRS STRTRS
#define DTZRQF STZRQF
 
/* double complex -> single complex */
 
#define IZMAX1 ICMAX1
#define ZBDSQR CBDSQR
#define ZGBBRD CGBBRD
#define ZGBCON CGBCON
#define ZGBEQU CGBEQU
#define ZGBRFS CGBRFS
#define ZGBSVX CGBSVX
#define ZGBTRF CGBTRF
#define ZGBTRS CGBTRS
#define ZGEBAK CGEBAK
#define ZGEBAL CGEBAL
#define ZGEBRD CGEBRD
#define ZGECON CGECON
#define ZGEEQU CGEEQU
#define ZGEES CGEES
#define ZGEESX CGEESX
#define ZGEEV CGEEV
#define ZGEEVX CGEEVX
#define ZGEGS CGEGS
#define ZGEGV CGEGV
#define ZGEHRD CGEHRD
#define ZGELQF CGELQF
#define ZGELS CGELS
#define ZGELSS CGELSS
#define ZGELSX CGELSX
#define ZGEQLF CGEQLF
#define ZGEQPF CGEQPF
#define ZGEQRF CGEQRF
#define ZGERFS CGERFS
#define ZGERQF CGERQF
#define ZGESVD CGESVD
#define ZGESVX CGESVX
#define ZGETRF CGETRF
#define ZGETRI CGETRI
#define ZGETRS CGETRS
#define ZGGBAK CGGBAK
#define ZGGBAL CGGBAL
#define ZGGGLM CGGGLM
#define ZGGHRD CGGHRD
#define ZGGQRF CGGQRF
#define ZGGRQF CGGRQF
#define ZGGSVD CGGSVD
#define ZGGSVP CGGSVP
#define ZGTCON CGTCON
#define ZGTRFS CGTRFS
#define ZGTTRF CGTTRF
#define ZGTTRS CGTTRS
#define ZHBEV CHBEV
#define ZHBEVD CHBEVD
#define ZHBEVX CHBEVX
#define ZHBGST CHBGST
#define ZHBGV CHBGV
#define ZHBTRD CHBTRD
#define ZHECON CHECON
#define ZHEEV CHEEV
#define ZHEEVD CHEEVD
#define ZHEEVX CHEEVX
#define ZHEGV CHEGV
#define ZHERFS CHERFS
#define ZHETRD CHETRD
#define ZHETRF CHETRF
#define ZHETRI CHETRI
#define ZHETRS CHETRS
#define ZHGEQZ CHGEQZ
#define ZHPCON CHPCON
#define ZHPEV CHPEV
#define ZHPEVD CHPEVD
#define ZHPEVX CHPEVX
#define ZHPGV CHPGV
#define ZHPRFS CHPRFS
#define ZHPTRD CHPTRD
#define ZHPTRF CHPTRF
#define ZHPTRI CHPTRI
#define ZHPTRS CHPTRS
#define ZHSEIN CHSEIN
#define ZHSEQR CHSEQR
#define ZPBCON CPBCON
#define ZPBEQU CPBEQU
#define ZPBRFS CPBRFS
#define ZPBSTF CPBSTF
#define ZPBTRF CPBTRF
#define ZPBTRS CPBTRS
#define ZPOCON CPOCON
#define ZPOEQU CPOEQU
#define ZPORFS CPORFS
#define ZPOTRF CPOTRF
#define ZPOTRI CPOTRI
#define ZPOTRS CPOTRS
#define ZPPCON CPPCON
#define ZPPEQU CPPEQU
#define ZPPRFS CPPRFS
#define ZPPTRF CPPTRF
#define ZPPTRI CPPTRI
#define ZPPTRS CPPTRS
#define ZPTCON CPTCON
#define ZPTEQR CPTEQR
#define ZPTRFS CPTRFS
#define ZPTTRF CPTTRF
#define ZPTTRS CPTTRS
#define ZSPCON CSPCON
#define ZSPRFS CSPRFS
#define ZSPTRF CSPTRF
#define ZSPTRI CSPTRI
#define ZSPTRS CSPTRS
#define ZSTEDC CSTEDC
#define ZSTEIN CSTEIN
#define ZSTEQR CSTEQR
#define ZSYCON CSYCON
#define ZSYRFS CSYRFS
#define ZSYTRF CSYTRF
#define ZSYTRI CSYTRI
#define ZSYTRS CSYTRS
#define ZTBCON CTBCON
#define ZTBRFS CTBRFS
#define ZTBTRS CTBTRS
#define ZTGEVC CTGEVC
#define ZTGSJA CTGSJA
#define ZTPCON CTPCON
#define ZTPRFS CTPRFS
#define ZTPTRI CTPTRI
#define ZTPTRS CTPTRS
#define ZTRCON CTRCON
#define ZTREVC CTREVC
#define ZTREXC CTREXC
#define ZTRRFS CTRRFS
#define ZTRSEN CTRSEN
#define ZTRSNA CTRSNA
#define ZTRSYL CTRSYL
#define ZTRTRI CTRTRI
#define ZTRTRS CTRTRS
#define ZTZRQF CTZRQF
#define ZUNGBR CUNGBR
#define ZUNGHR CUNGHR
#define ZUNGLQ CUNGLQ
#define ZUNGQR CUNGQR
#define ZUNMBR CUNMBR
#define ZUNMHR CUNMHR
#define ZUNMLQ CUNMLQ
#define ZUNMQR CUNMQR
#define ZUNMRQ CUNMRQ
#define ZUNMTR CUNMTR
#define ZUPGTR CUPGTR
#endif /* SINGLE */
 
#endif /* _LAPACK__ */