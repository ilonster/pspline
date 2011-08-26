! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     File: Wkary.inc                            starts
!     scratch, miscelaneous arrays
      INTEGER MxPLTZON
      REAL                                                              &
     &     wkzr(NZRDIM), ilist(NVELDIM),                                &
     &     wkv(NWKVDIM), wkpsi(NPSIDIM), wkzx(ZXDIM)
      PARAMETER (MxPLTZON=NPLTDIM+NZONDIM)
      REAL                                                              &
     &     xp(MxPLTZON),  yp(MxPLTZON), xxp(MxPLTZON), yyp(MxPLTZON)
      COMMON /lsc_wkcom0/ wkzr, wkzx, wkv, wkpsi, ilist, xp, yp,          &
     &     xxp, yyp
!
!     note;  After April 16, 1993 the nwkvdim parameter is active
!            and this line is replaced:
!    ^     wkv(NVELDIM), wkpsi(NPSIDIM), wkzx(ZXDIM)
!     note;  Also, the parameter MxPLTZON introduced for plotting arrays
!     File: Wkary.inc                            ends
