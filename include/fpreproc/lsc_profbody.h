! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     File: ProfBody.inc                        starts
!     Commentary on --Vec --Ary dVlVec dVlAry dVol:
!     --Vec refers to a vector on the TSC psi grid
!     --Ary refers to a vector on the LSC psi grid
!     dVlVec(j) is the volume between j-1 and j because vptemp
!     is a backward-calculated quantity.  iVlVec is the sum of dVlVec
!     dVol(j) is the volume centered on j
      REAL                                                              &
     &        NeAry(NPSIDIM), PsiAry(NPSIDIM),  TeAry(NPSIDIM),         &
     &        ZbrAry(NPSIDIM),iVlAry(NPSIDIM),                          &
     &        dVol(NPSIDIM),  EdcAry(NPSIDIM), LnlAry(NPSIDIM),         &
     &        BetZAry(NPSIDIM), MidAry(NPSIDIM) 
      COMMON /lsc_prcom/                                                    &
     &        NeAry, PsiAry, TeAry,                                     &
     &        ZbrAry,iVlAry,                                            &
     &        dVol, EdcAry, LnlAry, BetZAry, MidAry
      REAL                                                              &
     &        psiminx, psimaxx, delpsi, Te, Ne, Zbr, pe2min
      COMMON /lsc_plascom/                                                &
     &        psiminx, psimaxx, delpsi, Te, Ne, Zbr, pe2min
!     File: ProfBody.inc                        ends
