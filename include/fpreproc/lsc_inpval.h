! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     NameList begins                       ---------------------------|
!                                                                      |
!                                                                      |
      NAMELIST /inpvalue/                                               &
     &            fghz,   ntors,    npols,     nrays,    nGrps,         &
     &         nparmax,  nparmin,  npolmin,  npolmax,                   &
     &         centers, couplers,   widths,   powers, phaseDeg,         &
     &          DoBram,  nslices,                                       &
     &        DiffuJrf, PrfSpred
!
      NAMELIST /inpexprt/                                               &
     &          HstpLH,    nstep,    nfreq,     npsi,   nzones,         &
     &              nv,    nsmoo,     nsmw,                             &
     &         nRampUp,    nFlat, WeghtItr,                             &
     &           PlFlg,   PrFlg ,    idiag,                             &
     &          DoXcam,   Do1Rpr,   Do0Edc,                             &
     &        TailTeps, TailPeps, TailNeps,                             &
     &        ScatKdeg, TurnNegs,   DoTRAN,                             &
     &           thet0
!                                                                      |
!                                                                      |
!     NameList ends                         ---------------------------|
!
! EXAMPLE FILE
!  &inpvalue 
!  fghz = 4.6, 
!  nGrps = 1,
!  powers(1) = 1.00, powers(2) = 1.00,
!  phaseDeg(1) = 130., phaseDeg(2) = 130., nslices=301,
!  ntors=15, npols=1,
!  DoBram = 1, 
!  couplers(1)='TFTRLHCD',
!  couplers(2)='TFTRLHCD',
!  DiffuJrf = 0.00, 
!  /
! 
!  &inpexprt
!  HstpLH = .01,  nstep = 20000,  nfreq = 50,  npsi = 100,
!  nzones = 2000,
!  nv = 401, nsmoo = 9, nsmw = 3,  weghtitr = .2,
!  plflg = 0, 0, 0, 1, 0, 0, 0, 
!  prflg = 0, 0, 0,
!  /
