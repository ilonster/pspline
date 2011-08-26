! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     File: params.inc                          starts
!     Name:   Typical:
!     NZONDIM  2000 
!             max number of ray zones; ray not followed further than
!             this number of zone crossings
!     NPLTDIM  800
!             plotting dimension
!     NRAYDIM  210
!             max number of rays
!     NTORDIM   30
!             max number of distinct n_parallels meaning n_phi-s toridally
!     NPOLDIM    7
!             max number of distinct n_poloidals which stir in to n_parallel
!     NGRPDIM    3  
!             max number of groups in the spectrum
!     NVELDIM  401
!             maximum number of parallel velocity zones
!     NSMODEF 
!             default value of nsmoo
!     NSMWDEF
!             default value of nsmw
!     NPSIDIM  100  
!             max size of spatial grid in psi
!     NITRDIM
!             RAYARRAY        dimensioned (NZONDIM, NRAYDIM)
!             VQLARRAY        dimensioned (NVELDIM, NPSIDIM)
!     NITRDIM 2       
!             number of copies of fe kept
!     NWKARDIM = NRAYDIM * NZONDIM * 3  !! obsolete and taken out Apr9,93
!
!                                       The relative size of these dimensions
!                                       is implicitly assumed in work arrays.
!                                       NVELDIM >= NPSIDIM + NZONDIM 
!                                       eXcept, After April 16, 1993, this 
!                                       restriction removed by the introduction
!                                       of NWKVDIM==NVELDIM+NPSIDIM+NZONDIM
 
      INTEGER INUNIT, OUTUNIT, NWINDIM, FALSE, TRUE
      PARAMETER(INUNIT = 3, OUTUNIT = 4, NWINDIM = 4)
      PARAMETER(FALSE = 0, TRUE = 1)
 
      INTEGER PIMP, PPSI, PNX, PNZ, PWORDS
      PARAMETER (PIMP=3, PPSI= 401, PNX= 125, PNZ= 159)
      PARAMETER (PWORDS = 10)
      INTEGER NPSIDIM, NGRPDIM, NVELDIM, NSMODEF, NSMWDEF
! max number of coupler *types*
      INTEGER NCPLDIM
      PARAMETER(NCPLDIM=10)
!
      INTEGER NTORDIM, NPOLDIM, NRAYDIM
      INTEGER NZONDIM, NPLTDIM, NWKVDIM
      PARAMETER(NPSIDIM  = 100, NGRPDIM = 3) 
      PARAMETER(NVELDIM  = 401, NZONDIM = 2000)
      PARAMETER(NSMODEF  =   9, NSMWDEF = 3)
      PARAMETER(NTORDIM=30, NPOLDIM=7, NRAYDIM=NTORDIM*NPOLDIM)
      PARAMETER( NPLTDIM = 800)
      INTEGER ZXDIM, NPZDIM
      PARAMETER(ZXDIM = 2 * PNZ * PNX)
      INTEGER NZRDIM, NVPDIM
      PARAMETER(NZRDIM = 3 * NRAYDIM * NZONDIM,                         &
     &          NVPDIM = 2 * NVELDIM * NPSIDIM,                         &
     &          NPZDIM = 2 * NPSIDIM * NZONDIM)
      INTEGER NITRDIM
      PARAMETER(NITRDIM = 2)
      PARAMETER(NWKVDIM = NVELDIM+NPSIDIM+NZONDIM)
!     File: params.inc                          ends
