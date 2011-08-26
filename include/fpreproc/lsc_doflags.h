! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     file:  Doflags.inc begins
!
!     Contains compute switches and global io switches
!
!     DoBram = 1 computes spectrum from JEStevens Brambilla code
!              0 makes a spectrum out of arbitrary Gaussians
!     DoTRAN = 1 LSC being called by TRANSP, which passes namelist variables
!                thru commons
!     DoXcam = 1 give pictures and plots like the 2d x ray camera
!     Do1Rpr = 1 retrace just 1 ray per call with iRayTrs = 1, after the 
!                first call; Do1RayPerCall
!     Do0Edc = 1 zero out Edc as read from lhcdoua/jardin.d using flag
!                iEdc and value EdcInp in Escan.inc
      INTEGER        DoBram, DoTRAN,                                    &
     &               DoXcam, Do1Rpr, Do0Edc
      COMMON /lsc_DoCom0/DoBram, DoTRAN,                                    &
     &               DoXcam, Do1Rpr, Do0Edc
!
!     file:  Doflags.inc ends
