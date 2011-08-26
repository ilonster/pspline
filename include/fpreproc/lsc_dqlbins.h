! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     DqlBins ---------------------------------------------------------|
!                                                                      |
!                                                                      |
!     File: DqlBins,inc starts
!     Dql     REAL    VQLARRAY
!             (iv, ipsi)
!             electron quasilinear diffusion coefficient indexed
!             on parallel velocity and space
!     Smoothing provided by nsmoo and nwmw accorting to
!     qlsm = (normalization) exp [ -(iv - imax)^2/(2 nsmw^2) ]
!     Values of qlsm at edge of smoothing vector are:
!     nsmw=1; nsmoo=3, 1/root(e); nsmoo=5, 1/e^2; nsmoo=7, 1/e^4.5 = 1/100
!     
!     How much smoothing? Suppose NV=1999, 1000 bins from zero to light speed.
!     Suppose T= 1--3 kV; n_par = 2--5
!      v_T/c   40 --  80 bin
!     3 v_T/c 120 -- 240 bin
!     1/n_par 200 -- 500 bin
!     nsmw     ?? 30   ?? 10 min
!     nsmoo    ?? 75   ?? 25 
      INTEGER DqlBox(4), nsmoo, nsmw, nsmsym
      REAL                                                              &
     &        DcollNorm, nuNorm, DqlHite, Pwrnorm, DqlNorm
      REAL                                                              &
     &        Dql(NVELDIM, NPSIDIM,2),                                  &
     &        Dcoll(NVELDIM, NPSIDIM), nuColl(NVELDIM, NPSIDIM),        &
     &        qlsm(NVELDIM)
      COMMON /lsc_DqlCom0/                                                  &
     &        DcollNorm, nuNorm, DqlHite, Pwrnorm, DqlNorm,             &
     &        Dql                  ,                                    &
     &        Dcoll                  , nuColl ,                         &
     &        qlsm
 
      COMMON /lsc_DqlCom1/                                                  &
     &        DqlBox, nsmoo, nsmw, nsmsym
!     Dql is the quasilinear diffusion coefficient
!     Dql(v,psi,1) is the UNsmoothed version
!     Dql(v,psi,2) is the   SMOOTHED version
!     Dcoll is the collisional diffusion coefficient
!     nuColl is the collisional drag coefficient
!                                                                      |
!                                                                      |
!     DqlBins ---------------------------------------------------------|
