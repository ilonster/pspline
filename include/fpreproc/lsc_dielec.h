! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     dielec.inc:contains mostly dielectric properties of plasma ------|
!                                                                      |
!                                                                      |
!     NEQS is the number of equations integrated for the ray:
!     r; z; phi; k_r; k_z; k_phi=n_phi; and time.  
!     n_phi is conserved, so this is not needed.
!     time is used in calculating E_z, thru the time spent is a zone.
!
      INTEGER NEQS, NEQSP1
      PARAMETER (NEQS=7,NEQSP1=8)
      REAL    f(NEQSP1),f1(NEQS),f2(NEQS),f3(NEQS),                     &
     &        y(NEQSP1),y1(NEQS),y2(NEQS),y3(NEQS)
      COMMON /lsc_IGRAT1/ f,y
      COMMON /lsc_IGRAT2/ f1,f2,f3,y1,y2,y3
 
      INTEGER IDM
      PARAMETER (IDM = NZONDIM)
      INTEGER iplt
      REAL    d1,d2,d4
      REAL    denom,DKpar,DKper,wdDdw,dDdkABS,epQL, epsL, epsz
      REAL    yrr(IDM),yzz(IDM),ypp(IDM),                               &
     &        ypr(IDM),ypl(IDM),ypw(IDM),                               &
     &        yed(IDM),yps(IDM),                                        &
     &        y1old   ,y2old   ,y3old   
 
      REAL    Eper , Epar , Exy  , Aion , Aelc , OmEC2,                 &
     &        EparI, EparIK, cEparIK
 
      REAL    D11er, D33er, D12er,                                      &
     &        D11ar, D33ar, D12ar,                                      &
     &        D11w0, D33w0, D12w0
 
      COMMON /lsc_DNORM/ d1,d2,d4
      COMMON /lsc_DAMP/                                                   &
     &        denom,DKpar,DKper,wdDdw,dDdkABS,epQL, epsL, epsz
      COMMON/lsc_PltBlk/                                                    &
     &        yrr,yzz,ypp,                                              &
     &        ypr,ypl,ypw,                                              &
     &        yed,yps,                                                  &
     &        y1old   ,y2old   ,y3old   ,                               &
     &        iplt
 
!      yrr:r       yzz:z      ypp:\phi    yps: sqrt( psi_{norm} )
!      ypr:k_\perp ypl:n_\parallel
!      ypw:pwr(s)  power remaining
!      yed:electon damping term
!      \Im{k_\perp} / \Re{k_\perp}
 
      Common /lsc_EPS1/                                                    &
     &        Eper , Epar , Exy  , Aion , Aelc , OmEC2,                 &
     &        Epari, EparIK, cEparIK
      Common /lsc_EPS2/                                                    &
     &        D11er, D33er, D12er,                                      &
     &        D11ar, D33ar, D12ar,                                      &
     &        D11w0, D33w0, D12w0
 
!     Blocks EPS give dielectric tensor elements and their derivatives.
!     Using Stix notation of S (Kper) P(Kpar) D(Kxy) R L and
!     a = (wp/w)^2   b = (wce/w)^1
!     OmEC2  =  1. - b^2
!     we have
!     Lower Hybrid Case           Electron Cyclotron Case
! Eper       S                             S (1-b^2)
! Epar       P                             P
! Exy        D
! Erl                                     R L(1-b^2)
! Aion    Ion mode conversion
! Aelc    Electron term                  Electron mode conversion
!     The derivatives of the tensor elements are given in the
!     D terms.  Usage differs for ecrh and lhrh.
!
!                       K               K               K
!   ECRH                 PERP            RL              PAR
!
!  (K-PERP)**2          D11ER           DRLER           D33ER
 
!  (K-PAR )**2          D11AR           DRLAR           D33AR
 
!  (OMEGA )**2          D11w0           DRLw0           D33w0
!
!                       K               K               K
!    LHRH                PERP            XY              PAR
!
!  (K-PERP)**2          D11ER           D12ER           D33ER
 
!  (K-PAR )**2          D11AR           D12AR           D33AR
 
!  (OMEGA )**2          D11w0           D12w0           D33w0
!
!     iplt      index of plot array
!     y         function   array for integration
!             y(1   2   3   4   5   6   7   8)
!               r   z   phi kr  kz  n   wt  s
!     f         derivative of y by s, array for integration
!     f1,       derivative
!     f2,               array
!     f3,                     at old points
!     y1,       function
!     y2,                 array
!     y3                        at old points
!     d1,       terms
!     d2,             of dispersion
!     d4,                          relation
!     DKpar ==  \p D/\p k_{\parallel}^2
!     DKper ==  \p D/\p k_{\perp}^2
!     wdDdw ==  \omega \p D/\p \omega
!     dDdkABS == | \p D/{\p {\bf k}} |
!             == wdDdw * dsdwt
!                which is of interest in the damping calculation:
!     d {ln P}/ds = - 2 {\p D/\p Epar}/{dDdkABS} x Im{Epar}
!     where Im{Epar} =  PI ( \omega_p/k_{\par}v_{\par})^2
!                       (-v_{\par}^2 \p f_o/\p v) @ v=\omega/k_{\par}
!     if Maxwellian
!           Im{Epar} = PI^(1/2)(\omega_p/\omega)^2 2 \zeta^3 exp(-\zeta^2)
!           \zeta = \omega/(2^{1/2} k_{\par} v_t)^2
!     dlnPdsX == d ln(P)/ds \cdot ds if maXwellian, or at maXimum 
!                                       without quasilinear burnthrough.
!     dlnPdsX = -2 (\p D/\p Epar) / dDdkABS * Im{Epar} \times ds
!                                       where Epar is K_{zz} is K_{33}
!     dlnPdsK == d ln(P)/ds \cdot ds per f_e^{\prime}, or Kernel of
!                                       the damping decrement.
!     EparI   == Im{K_{zz}} assuming Maxwellian distribution
!     EparIK  == Im{K_{zz}} for unit value of  \p f_e/\p v_{\parallel}
!                                        Note: f_e contains electron density
!                                        and v_{\parallel} is normalized to c
!
!                                                                      |
!                                                                      |
!     dielec.inc:contains mostly dielectric properties of plasma ------|
