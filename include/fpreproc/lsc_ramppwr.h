! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     Ramppwr.inc begins
!     NrampDIM  Max number of different power levels in the 
!               ramp up process aimed at iterating to f_e(v_\parallel)
!     
!     NrampUp   Num of ramp ups including the flat portion starting from
!                    a Maxwellian
!     Nflat     Num of iterations during the ramp which are flat
!     PwrLevel  Array of power levels. First increasing; then flat.
      INTEGER NRAMPDIM, NFLATDEF
      PARAMETER(NRAMPDIM = 200)
      PARAMETER(NFLATDEF =  10)
      INTEGER nRampUp, nFlat
      REAL pwrlevel(NRAMPDIM), FeCvgAry(NRAMPDIM)
      COMMON /lsc_rampc0/ nRampUp, nFlat
      COMMON /lsc_rampc1/ pwrlevel, FeCvgAry
!     Ramppwr.inc ends
