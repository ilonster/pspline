! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!
!     .         --------------------------------------------------------
!     Xray.inc
!     E_photon        in MeV
!     E_incvec        in MeV
!     E_min E_max
      CHARACTER*2 FoilCode
      INTEGER nMUbins, nEbins, iAbsXray
      INTEGER Eint(NVELDIM)
      REAL                                                              &
     &        mu_min, mu_max, E_min, E_max, E_ph_min, dE_ph,            &
     &        dFoilTCM
      REAL    dmu_inv
      REAL                                                              &
     &        sigtot(NMUDIM, NENDIM), E_incvec(NENDIM), XmnFac(NENDIM), &
     &        muvec(NMUDIM), Efrac(NVELDIM), Eofv(NVELDIM),             &
     &        dEofv(NVELDIM)
      REAL                                                              &
     &        inten(NMUDIM, NPSIDIM) 
      COMMON /lsc_Xray1a/ nMUbins, nEbins, iAbsXray
      COMMON /lsc_Xray2b/ Eint
      COMMON /lsc_Xray3c/ sigtot, E_incvec, XmnFac, muvec, inten, Efrac
      COMMON /lsc_Xray4d/ E_ph_min, dE_ph
      COMMON /lsc_Xray5e/ dFoilTCM
      COMMON /lsc_Xray6f/ Eofv, dEofv
      COMMON /lsc_Xray7g/ mu_min, mu_max, dmu_inv, E_min, E_max
      COMMON /lsc_Xray8h/ FoilCode
!                       ! dFoilTCM; or; dcu: thickness of foil (cm)
!                       ! FoilCode; or; FC: CU,AG,TA,MO, or 00 for no foil
!     Xray.inc
!     .         --------------------------------------------------------
!
