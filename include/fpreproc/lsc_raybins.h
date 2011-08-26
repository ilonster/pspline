! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     RayBins                               ---------------------------|
!                                                                      |
!                                                                      |
!     nzones  INTEGER
!             number of zones (input)
!     nrays   INTEGER
!             number of rays (input)
!     npsiJ   INTEGER
!             number of spatial grid points (input from TSC);
!             .le. PPSI which sets the max array size of TSC.
!             The final  J  is for Jardin.  Equivalent to his   npsit  .
!     npsi    INTEGER
!             number of psi bins as used in Dql calculation; 
!             can be more or less than npsii from the TSC grid.
!             .le. NPSIDIM
!             NOT to be confused with: NpsiJ, npsit
!     izone   INTEGER
!             zone index; low --> starting of ray; high --> ending
!     iray    INTEGER
!             ray index; increases from low starting n_{\parallel}
!     iLastRy INTEGER
!             index of the last ray traced; used when tracing rays gradually.
!     ipsi    INTEGER
!             psi index; increases from low psi at center to psimax at edge
!     ivind   INTEGER RAYARRY
!             table of index into Vpar along each ray
!     izind   INTEGER RAYARRAY                                           ejv
!             izind(izone, iray) maps ray intersection number
!             to spatial index
!             As a ray progresses from the outside in, izone increases
!             by one each time a psi zone is crossed.  Typically, izind( , ):
!
!                    RAY#1   RAY#2   RAY#3   RAY#4   RAY#5   RAY#6
!             ZONE#1   61      61      61      61      61      61
!             ZONE#2   60      60      60      60      60      60
!             ZONE#3   59      59      59      59      61      59
!             ZONE#4   58      58      58      60      --      58
!             ZONE#5   57      57      59      61              57
!             ZONE#6   56      58      60      --              56
!             ZONE#7   55      59      61                      57
!
!     iznew
!     izold   are not arrays, but are related to izind( , ).  As the ray
!             progresses, iznew is computed and compared to izold. If there
!             is a change, then a zone has been passed, and information
!             is stored.
!     power   REAL    RAYARRAY
!             the power on each ray as a function of zone number
!             Indexing: (izone, iray)
!             psi, etc at the zones intersection
!             = psi(izind(izone, iray))
!             Units:  Watts
!     dlnPdsK REAL    RAYARRAY
!             d ln(P)/ds for unit value of { \p f_e/\p v_{\parallel} }
!     dlnPdsX REAL    RAYARRAY 
!             d ln(P)/ds if maXwellian, or at maXimum 
!                                          without quasilinear burnthrough.
!             = -2 (\p D/\p Epar) / dDdkABS * Im{Epar}
!                where Epar is K_{zz} is K_{33}
!             Units: m^{-1}
!     dlnPds  REAL    RAYARRAY
!             d ln(P)/ds after quasilinear effects,
!             always larger than dlnPdsX by the thermal slope to ql slope
!             Units: m^{-1} 
!     ezsq    REAL    RAYARRAY
!             (izone, iray)
!             ratio of square of parallel electric field
!             to power flowing along ray
!             Units: MKS -- (volts/meter)^2 / watts
!     npar    REAL    RAYARRAY
!             (izone, iray)
!             c_light k_parallel / omega (parallel index)
!     dnpar   REAL
!             scalar n_parallel with for each ray
!             used in Dql computation; computed from input
!     vpar    REAL    RAYARRAY
!             (izone, iray)
!             parallel phase velocity.  Used in q-l calculation.
!             Units: v_parallel / c_light
!     spectrumREAL
!             function which returns normalized power given n_parallel.
!             integral (spectrum(n) * dn) = 1.
!     spwidth REAL
!             scalar n_parallel spectral width (input variable).
!             Used by spectrum
!     spcentr REAL
!             central n_parallel value (input).
!             Used by spectrum
!     npargr  SUBROUTINE
!             function which generates initial nparallel grid
!             of size nray extending from nparmin to nparmax
!     nparmin REAL
!             initial minimum nparallel actually assigned to a ray
!     nparmax REAL
!             initial maximum nparallel actually assigned to a ray
!     rFudgDmp REAL
!             (izone, iray)    --- real to Fudge Damping
!             factor less than 1 showing the average power in a zone.
!             This factor is created in the ray picture, and is 
!             communicated to the QL picture. I
!             If d is the damping decrement, ie, exp(-d), then
!             rFudgDmp = < exp ( -d ) >
!             rFudgDmp = (1 - exp(-d) ) / d  =~ 1 - d/2 + d^2/6 for d small
!
!     P(iz+1,ir) = P(iz,ir) exp ( -d )
!     Dql = P(iz,ir) \eta  v /( dvsym dVol )  * rFudgDmp
!     and, hopefully, Pql == Pray
!     note, \eta is the polarization term .... dD/dEpar / (omega dD/dw ) with
!     constants added.
!
!     Here follow quantities of interest for diagnostics and understanding
!     but not required for the quasilinear calculation:
!     RofRay (NZONDIM) radius location of ray vs zone number
!     ZofRay (NZONDIM) z (height)                " "
!     PofRay (NZONDIM) Phi (toroidal angle)      " "
!     NperRy (NZONDIM) n_{\perp}                 " "
!     NparRy (NZONDIM) n_{\parallel}             " " 
!     rtPsRy (NZONDIM) {\psi}^0.5                " "
!     NeofRy (NZONDIM) n_e                    vs zone number
!     BthRay (NZONDIM) B_\theta               vs zone number
!     BphRay (NZONDIM) B_\phi                 vs zone number
!     PowrRy (NZONDIM) exp(-2\int k_i \cdot dr)  " "  (linear damping)
!     TimeRy (NZONDIM) time given as \omega t    " "
!     DistRy (NZONDIM) distance in meters        " "  
!     DetrRy (NZONDIM) determinate D over largest term (relative error) ""
!     Diagnostic quantities end.
      INTEGER                                                           &
     &        ipsi, iray, iLastRy, iznew, izold,                        &
     &        izone,                                                    &
     &        lnewray,                                                  &
     &        npsi, nrays,                                              &
     &        nzones, nrl, nru, nzl, nzu, nslices,                      &
     &        izind(NZONDIM, NRAYDIM), ivind(NZONDIM, NRAYDIM),         &
     &        izcind(NZONDIM, NRAYDIM)
 
      REAL                                                              &
     &        dnpar, dtdV,                                              &
     &        ezsq   (NZONDIM, NRAYDIM),                                &
     &        npar   (NZONDIM, NRAYDIM),                                &
     &        power  (NZONDIM, NRAYDIM),                                &
     &        dlnPds (NZONDIM, NRAYDIM),                                &
     &        dlnPdsK(NZONDIM, NRAYDIM),                                &
     &        dlnPdsX(NZONDIM, NRAYDIM), rFudgDmp(NZONDIM,NRAYDIM)
      REAL                                                              &
     &        RofRay (NZONDIM), ZofRay (NZONDIM), PofRay (NZONDIM),     &
     &        NparRy (NZONDIM), NperRy (NZONDIM), rtPsRy (NZONDIM),     &
     &        PowrRy (NZONDIM),                                         &
     &        TimeRy (NZONDIM), DistRy (NZONDIM), DetrRy (NZONDIM),     &
     &        NeofRy (NZONDIM), BthRay (NZONDIM), BphRay (NZONDIM)
  
      COMMON /lsc_rayIbin/                                                  &
     &        ipsi, iray, iLastRy, izind, izcind, ivind,                &
     &        iznew, izold, izone,                                      &
     &        lnewray,                                                  &
     &        npsi, nrays,                                              &
     &        nzones, nrl, nru, nzl, nzu, nslices
      COMMON /lsc_rayRbin/                                                 &
     &        dnpar           , dtdV            ,                       &
     &        ezsq            ,                                         &
     &        npar            ,                                         &
     &        power           ,                                         &
     &        dlnPds          , dlnPdsK         , dlnPdsX         ,     &
     &        rFudgDmp        ,                                         &
     &        RofRay          , ZofRay          , PofRay          ,     &
     &        NparRy          , NperRy          , rtPsRy          ,     &
     &        PowrRy          ,                                         &
     &        TimeRy          , DistRy          , DetrRy          ,     &
     &        NeofRy          , BthRay          , BphRay          
!
!     rayIbin   common block for integer output
!     rayRbin   common block for real output arrays
!
!                                                                      |
!     RayBins ends                          ---------------------------|
 
