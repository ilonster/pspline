! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     tscunits.inc      ------------------------------------------------
!
!     nTSCwrit          Unit number to which TSC writes equilibrium data
!     nTSCread          Unit number from which TSC reads Prf, Jrf , etc
!     nTSCscrn          Unit number TSC uses for writing to the screen
!     nTSCgraf          Unit number to which LSC writes gnuplot stuff
!                         for each graph
!     nLSCcomm          Unit number to which LSC sends 'communication' output
!     nTSCunus          Unit number not used by TSC; used internally by LSC
!  ORIGINAL USAGE of iRayTrsi:
!     iRayTrsi     0 use old ray data; recalculate f_e(v)
!                    taking account of new n_e and T_e,
!                    use new E_dc for the current
!	           1 calculate new rays, and f_e(v), from new equilibrium
!  NEW USAGE of iRayTrsi:
!     iRayTrsi     0 use old ray data, and old f_e(v);
!                    use new E_dc for the current
!	           1 calculate new rays, and f_e(v), from new equilibrium
!	           2 use old ray data, but calculate new f_e(v) 
!                    taking account of new n_e and T_e,
!                    use new E_dc for the current
!                         
!     iPlotsig          0 do not make plots; 1 make plots as in input.lhh
!     iError            0 LSC finishes without errors; 1 (or more) errors found
!                      -1 LSC found an error; calling program can keep going
!     iEndRy            Number of aborted rays on this call; failed restart of
!                       error encountered in path, such as too short wavelength
!     nLSCcom2          Alternate for LSC communication
      INTEGER         nTSCwrit, nTSCread, nTSCscrn,                     &
     &                nTSCgraf, nLSCcomm, nTSCunus,                     &
     &                iRayTrsi, iPlotsig, iXraysi, iError, iEndRy
      INTEGER         nLSCcom2
      COMMON/lsc_tscunits/nTSCwrit, nTSCread, nTSCscrn,                     &
     &                nTSCgraf, nLSCcomm, nTSCunus,                     &
     &                iRayTrsi, iPlotsig, iXraysi, iError, iEndRy,      &
     &                nLSCcom2
!
!     tscunits.inc      ------------------------------------------------
