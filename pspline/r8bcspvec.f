      subroutine r8bcspvec(ict,ivec,xvec,yvec,ivd,fval,
     >   nx,xpkg,ny,ypkg,fspl,inf3,
     >   iwarn,ier)
c
c  vectorized spline evaluation routine -- 2d spline
c  1.  call vectorized zone lookup routine
c  2.  call vectorized spline evaluation routine
c
c--------------------------
c  input:
!============
! idecl:  explicitize implicit INTEGER declarations:
      IMPLICIT NONE
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
      INTEGER iwarn1,iwarn2
!============
      integer ict(6)                    ! selector:
c        ict(1)=1 for f      (don't evaluate if ict(1)=0)
c        ict(2)=1 for df/dx  (don't evaluate if ict(2)=0)
c        ict(3)=1 for df/dy  (don't evaluate if ict(3)=0)
c        ict(4)=1 for d2f/dx2 (don't evaluate if ict(4)=0)
c        ict(5)=1 for d2f/dy2 (don't evaluate if ict(5)=0)
c        ict(6)=1 for d2f/dxdy (don't evaluate if ict(6)=0)
c
      integer ivec                      ! vector dimensioning
c
c    ivec-- number of vector pts (spline values to look up)
c
c  list of (x,y) pairs:
c
      REAL*8 xvec(ivec)                   ! x-locations at which to evaluate
      REAL*8 yvec(ivec)                   ! y-locations at which to evaluate
c
      integer ivd                       ! 1st dimension of output array
c
c    ivd -- 1st dimension of fval, .ge.ivec
c
c output:
      REAL*8 fval(ivd,*)                  ! output array
c
c  fval(1:ivec,1) -- values as per 1st non-zero ict(...) element
c  fval(1:ivec,2) -- values as per 2nd non-zero ict(...) element
c   --etc--
c
c input:
      integer nx,ny                     ! dimension of spline grids
      REAL*8 xpkg(nx,4)                   ! x grid "package" (cf genxpkg)
      REAL*8 ypkg(ny,4)                   ! y grid "package" (cf genxpkg)
      integer inf3                      ! fspl 3rd array dimension, .ge.nx
      REAL*8 fspl(4,4,inf3,ny)            ! (non-compact) spline coefficients
c
c output:
c condition codes, 0 = normal return
      integer iwarn                     ! =1 if an x value was out of range
      integer ier                       ! =1 if argument error detected
c
c---------------------------------------------------------------
c  local arrays
c
      integer ix(ivec)                  ! x zone indices
      REAL*8 dxv(ivec)                    ! x displacements w/in zones
      integer iy(ivec)                  ! y zone indices
      REAL*8 dyv(ivec)                    ! y displacements w/in zones
c
c---------------------------------------------------------------
c
c  error checks
c
      ier=0
c
      if(nx.lt.4) then
         write(6,*) ' ?bcspvec:  nx.lt.4:  nx = ',nx
         ier=1
      endif
c
      if(ny.lt.4) then
         write(6,*) ' ?bcspvec:  ny.lt.4:  ny = ',ny
         ier=1
      endif
c
      if(ivec.le.0) then
         write(6,*) ' ?bcspvec:  vector dimension .le. 0:  ivec = ',
     >      ivec
         ier=1
      endif
c
      if(ivd.lt.ivec) then
         write(6,*)
     >      ' ?bcspvec:  output vector dimension less than input ',
     >      'vector dimension.'
         write(6,*) ' ivec=',ivec,' ivd=',ivd
         ier=1
      endif
c
      if(ier.ne.0) return
c
c  vectorized lookups
c
      ix=0
      iy=0
      call r8xlookup(ivec,xvec,nx,xpkg,1,ix,dxv,dxv,dxv,iwarn1)
      call r8xlookup(ivec,yvec,ny,ypkg,1,iy,dyv,dyv,dyv,iwarn2)
      iwarn=max(iwarn1,iwarn2)
c
c  vectorized evaluation
c
      call r8bcspevfn(ict,ivec,ivd,fval,ix,iy,dxv,dyv,fspl,inf3,ny)
c
      return
      end
