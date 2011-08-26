! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     FeBins.inc ------------------------------------------------------|
!                                                                      |
!                                                                      |
!     dvsym   REAL    VARRAY
!             delta v s figured symmetrically
!     dvisym  REAL    VARRAY
!             inverse of the delta v s figured symmetrically
!     nv      INTEGER
!             actual number of parallel velocity zones (input)
!     fe      REAL    VQLARRAY
!             (iv, ipsi, iitr)
!             electron distribution function indexed
!             on parallel velocity and space.  Velocity is normalized to
!             c, speed of light.  Integral of fe over v gives density in
!             cm^{-3}.  Jardin data file passes density in this form.
!     FstFracN(NPSIDIM)
!             The fraction of electrons that can be called fast particles
!             for their location because of interaction with the wave.
!             \integral_-1^1 ( fe(v/c,\psi) - FeMaxw(v/c,\psi) ) dv/c
!             divided by
!             \integral_1^1 FeMaxw(v/c,\psi) dv/c
!     FstFracE(NPSIDIM)
!             The fraction of electron energy due to fast particles
!             defined analogously to FstFracN but with v^2 weight.
!     dfdv    (iv, ipsi, 1) un smoothed df/dv
!     dfdv    (iv, ipsi, 2)    smoothed df/dv
!     .
!     TailPeps  Regarding a ficticious fast electron tail in both
!     TailNeps  directions, the fraction eps(ilon) for Pressure, Ne and Te
!     TailTeps  such that 
!     .                  TailNeps = TailPeps * TailTeps
!     .                  TailTeps = T_thermal/T_fast
!     .                  TailPeps = (n_fast T_fast)/(n_thermal T_thermal)
!     .         and
!     .              f(v) = (2 pi v_t^2)^-.5 1. n_e exp[- v^2/(2 v_t^2)]; all v
!     .         f_fast(v) = (2 pi v_f^2)^-.5 1. n_f exp[- v^2/(2 v_f^2)]; all v
!     .
!     .         where n_f/n_e = TailNeps
!     .               v_t/v_f = TailTeps^.5
!     .               n_f v_f^2 / [ n_t v_t^2 ] = TailPeps
!     .
!     TailVtrn  is the transition velocity relative to v_t at which the fast
!               electron tail becomes more important
!                        TailVtrn = v/v_t | transition
!     .                           = sqrt[2 ln(1/Neps 1/Teps^.50)/(1 - Teps^2)]
      INTEGER ivZero, nv, iITR, iSMO
      REAL    WeghtItr,                                                 &
     &        Vmin, Vmax, VthNorm,                                      &
     &        TeUnits, dv, fe0, nu0
!    ^        frv1minus, frv2minus, v1minus, v2minus, 
!    ^        frv1plus,   frv2plus, v1plus,  v2plus, epsvgr,
!    ^                             , betaZ, lnLambda
      REAL    TailPeps, TailNeps, TailTeps, TailVtrn
      REAL                                                              &
     &        Vpar(NVELDIM), fe(NVELDIM, NPSIDIM, NITRDIM),             &
     &        Vtherm(NPSIDIM), FeNorm(NPSIDIM),                         &
     &        VperpSq(NPSIDIM), dvsym(NVELDIM), dvisym(NVELDIM),        &
     &     dvip(NVELDIM), dvplus(NVELDIM),                              &
     &     dfdv(NVELDIM, NPSIDIM,2), nu0psi(NPSIDIM),                   &
     &     FstFracN(NPSIDIM), FstFracE(NPSIDIM)
      COMMON /lsc_feIbin/                                                   &
     &        ivZero, nv, iITR, iSMO
      COMMON /lsc_feRbin/ WeghtItr,                                         &
     &        Vmin, Vmax, VthNorm,                                      &
     &        TeUnits, dv, fe0, nu0,                                    &
     &        Vpar       , fe                , Vtherm         ,         &
     &         FeNorm,     VperpSq,                                     &
     &        dvsym, dvisym, dvplus, dvip,                              &
     &     dfdv, nu0psi,                                                &
     &     FstFracN, FstFracE,                                          &
     & TailPeps, TailNeps, TailTeps, TailVtrn
 
!     ivZero: index for which Vpar(ivZero) = 0.
!     iITR:   index for iteration on fe, either 1 or 2...NITRDIM
!     iSMO:   index for smoothing of Dql, either 1 (un) or 2 (smoothed)
!             note that Dql(v,p,1) goes with df/dv(v,p,2) and that 
!                       Dql(v,p,2) goes with df/dv(v,p,1) 
!     .                                 iSMO = 2 --> Do smoothing as
!     .                                 EValeo did in original code
!     .                                 using smoothed Dql=Dql(iv,ip,2)
!     .                                 iSMO = 1 --> 
!     .                                        Use unsmoothed Dql(.,.,1)
!     .                                 and smoothed dfdv(iv,ip) for 
!                                              Pql & Jrf
!
!     nv: total number of velocity grid points
!     Vpar:  parallel velocity / speed of light
!     vthermal:  thermal velocity / speed of light
!     fe:  electron velocity distribution function normalized so that
!     integral fe dVpar = electron number density cm-3
!     TeUnits:  normalization for electron temperature in keV
!                                                                      |
!                                                                      |
!     FeBins.inc ------------------------------------------------------|
