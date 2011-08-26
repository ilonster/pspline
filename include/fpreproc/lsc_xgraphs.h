! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!
!     xgraphs.inc --------------------------------------------------------
!     INTEGER*4 NWKDIx
      INTEGER   NWKDIx
      PARAMETER(NWKDIx = 1000)
!     INTEGER*4 iwkarx(NWKDIx)
      INTEGER   iwkarx(NWKDIx)
!     REAL*4 wkarx(NWKDIx, 4)
      REAL   wkarx(NWKDIx, 4)
      COMMON /lsc_grecom1/ iwkarx
      COMMON /lsc_grecom2/ wkarx
!     xgraphs.inc --------------------------------------------------------
!
