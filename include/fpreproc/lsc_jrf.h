! -*-f90-*-
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-23
!     Jrf.inc ---------------------------------------------------------
!     -                                                               |
!     -                                                               |
      INTEGER vnormNOK(NPSIDIM), ivrun
      REAL    vnorm, nuRuna, vnmax, gmrun, muminus, muplus, dEdcAmnt
      REAL    DiffuJrf, PrfSpred
      REAL    js(NPSIDIM), jsp(NPSIDIM), Jray(NVELDIM,NPSIDIM)
      REAL    nRunDot(NPSIDIM), jRunDot(NPSIDIM), vRunIdx(NPSIDIM)
      REAL    IrIntgrl(NPSIDIM), IpIntgrl(NPSIDIM)
      REAL    ugr(NVELDIM)
      REAL    vnormPos(NPSIDIM), vnormNeg(NPSIDIM)
      REAL    VparMaxP(NPSIDIM), VparMaxN(NPSIDIM)
!
      COMMON /lsc_JrfCom0/                                                 &
     &        vnorm, nuRuna, vnmax, gmrun, muminus, muplus, dEdcAmnt,   &
     &        DiffuJrf, PrfSpred,                                       &
     &        js,          jsp,        Jray,                            &
     &        nRunDot,     jRunDot,    vRunIdx,                         &
     &        IrIntgrl,          IpIntgrl,                              &
     &        ugr,                                                      &
     &        vnormPos,          vnormNeg,                              &
     &        VparMaxP,          VparMaxN
!
      COMMON /lsc_JrfCom1/ vnormNOK, ivrun
!
!     js        j_stopped.  See Karney and Fisch paper
!               current density deposited by the rf power
!     jsp       j_stopped if Edc were Edc + dE [= Edc + dEdcAmnt]
!               the p is for plus
!     Jray      js resolved by velocity bin and by psi bin;  
!               needed for a graph requested by BERNABEI October 94
!               which is implemented in JrfDiagn
!               
!     IrIntgrl  I from rays inside indexed psi surface 
!               found by Intgrl js
!     IpIntgrl  I as above but at increased E; Intgrl jsp
!     DiffuJrf  Arbitrary diffusion (smoothing) of Jrf in m^2/sec
!     PrfSpred  in range 0.0 --> 1.0 controls spreding of Prf to match
!               shape of n_e(psi) * J_{rf-smoothed}(psi)
!     ivrun     index of the velocity at which the v=v_runaway for
!               cooperative current drive; if zero, then does not happen
!     nRunDot   time rate of change of runaway population cm-3/sec
!     jRunDot   time rate of change of runaway current, from density 
!               increase
!               both above evaluated at the runaway velocity
!     vRunIdx   floating value of the integer iv index of the runaway 
!               velocity
!     -                                                               |
!     -                                                               |
!     -----------------------------------------------------------------
