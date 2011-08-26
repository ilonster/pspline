c  pspline library test routine
c
      subroutine pspltsub(filename,zctrl)
      implicit NONE
c
c  write output to file
c
      character*(*), intent(in) :: filename  ! write output here; ' ' for stdio
      real, intent(in) :: zctrl              ! control (not yet used...)
c
c  if filename ends in ".tmp" delete when done.
c
      common/bc_ctrl/ nbc
      common/pspltest_io/ m
c
      integer :: ilen,m,nbc
      logical :: tmpflag
c-----------------------------------
c
      tmpflag=.FALSE.
      if(filename.eq.' ') then
         m=6
      else
         m=99
         ilen=len(trim(filename))
         if(filename(max(1,(ilen-3)):ilen).eq.'.tmp') tmpflag = .TRUE.
c
         open(unit=m,file=filename,status='unknown')
      endif
c
c-----------------------------------
c  set nbc=0 to use "not a knot" instead of explicit 1st deriv bc
c  set nbc=1 to use explicit condition based on analytic expression
c  (more accurate)
c
      nbc=1
c
      write(m,1000)
 1000 format(/' *** 1d spline tests ***'/
     >'     f(x)=2+sin(x)'/
     >'     x in the interval [0,2pi]')
      call pspltest1(zctrl)
      write(m,1001)
 1001 format(/' *** 2d spline tests ***'/
     >'     f(x,theta)=exp(2x-1)*(2+sin(theta))'/
     >'     x in [0,1],  theta in [0,2pi]'/)
      call pspltest2(zctrl)
      write(m,1002)
 1002 format(/' *** 3d spline tests ***'/
     >'     f(x,theta,phi)=exp(2x-1)*(2+sin(theta))*(3+sin(phi))'/
     >'     x in [0,1],  theta in [0,2pi],  phi in [0,2pi]'/)
      call pspltest3(zctrl)
c
c-----------------------------------
c  close file
c
      if(tmpflag) then
         close(unit=m,status='delete')
      else
         close(unit=m)
      endif
c
c-----------------------------------
c
      return
      end
c
c------------------------------------------------
c
      subroutine pspltest1(zctrl)
c
      real, intent(in) :: zctrl
c
      common/pspltest_io/ m
c
      real x(80)
      real fs(320),fsp(320),xpkg(320)
      real fh(160),fs2(160)
c
      real xtest(1000),ftest(1000),zdum(1000)
      real testa1(1000),testa2(1000),testa3(1000),testa4(1000)
      real testa5(1000)
c
      real zcos(80),z2sin(80)
      real zero
c
      integer isize(4)
c
      data isize/10,20,40,80/
c
      data pi2/6.2831853/
      data zero/0.0/
c
c  test splining of function:  f(x)=2+sin(x)
c
c-------------------------------------------
c
      call tset(1000,xtest,ftest,zdum,zero-0.1,pi2+0.1)
c
      do is=1,4
         inum=isize(is)
         call tset(inum,x,z2sin,zcos,zero,pi2)
         do ix=1,inum
            zcos(ix)=cos(x(ix))         ! df/dx
         enddo
c
         write(m,*) ' '
         call dotest1(inum,x,z2sin,zcos,fs,fsp,fh,fs2,1000,
     >      xtest,ftest,xpkg,testa1,testa2,testa3,testa4,testa5,zdum)
      enddo
c
      return
      end
c------------------------------------------------
c
      subroutine dotest1(ns,x,f,fd,fspl,fspp,fherm,fs2,nt,xt,ft,xpkg,
     >   testa1,testa2,testa3,testa4,testa5,wk)
c
      use pspline_calls
c
      common/pspltest_io/ m
c
c  interpolant
c
      real x(ns)                        ! interpolant grid
      real f(ns)                        ! fcn values at grid pts
      real fd(ns)                       ! derivatives at grid pts
      real fspl(4,ns)                   ! spline coeff. array (calc. here)
      real fspp(4,ns)                   ! spline coeff. array (calc. here)
      real fherm(0:1,ns)                ! hermite array (calc. here)
      real fs2(0:1,ns)                  ! compact spline coeff. array calc here
c
      real xpkg(ns,4)                   ! x "package"
c
      real wk(nt)                       ! workspace
c
      real xt(nt)                       ! test grid
      real ft(nt)                       ! fcn values at test grid pts
c
      real testa1(nt),testa2(nt),testa3(nt),testa4(nt),testa5(nt)
c
      integer ict(3)
c
      common/bc_ctrl/ nbc
c
      data ict/1,0,0/                   ! just evaluate fcn value
c
c-------------------
c
      do i=1,ns
         fherm(0,i)=f(i)
         fherm(1,i)=fd(i)
         fspl(1,i)=f(i)
         fspp(1,i)=f(i)
         fs2(0,i)=f(i)
      enddo
c
      call genxpkg(ns,x,xpkg,1,1,1,4.0e-7,1,ierg)
      if(ierg.ne.0) write(m,*) ' ??dotest1:  genxpkg:  ierg=',ierg
c
c  spline setup calls
c
      call cspline(x,ns,fspl,nbc,fd(1),nbc,fd(ns),wk,nt,ilinx,ier) ! 1st deriv bc
      call cspline(x,ns,fspp,-1,zdum,-1,zdum,wk,nt,ilinx,ier) ! periodic
c
      call mkspline(x,ns,fs2,nbc,fd(1),nbc,fd(ns),ilinx,ier)
c
      call akherm1p(x,ns,fherm,ilinx,1,ier)
c
c  vectorize spline/hermite evaluation calls
c
      call spgrid(xt,nt,testa1,ns,xpkg,fspl,iwarn,ier)
      if(iwarn.ne.0) write(m,*) ' ?dotest1:  spgrid(1):  iwarn=',iwarn
      if(ier.ne.0) write(m,*) ' ?dotest1:  spgrid(1):  ier=',ier
c
      call spgrid(xt,nt,testa2,ns,xpkg,fspp,iwarn,ier)
      if(iwarn.ne.0) write(m,*) ' ?dotest1:  spgrid(2):  iwarn=',iwarn
      if(ier.ne.0) write(m,*) ' ?dotest1:  spgrid(2):  ier=',ier
c
      call gridspline(xt,nt,testa3,ns,xpkg,fs2,iwarn,ier)
      if(iwarn.ne.0) write(m,*) ' ?dotest1:  gridspline:  iwarn=',
     >   iwarn
      if(ier.ne.0) write(m,*) ' ?dotest1:  gridspline:  ier=',ier
c
      call gridherm1(xt,nt,testa4,ns,xpkg,fherm,iwarn,ier)
      if(iwarn.ne.0) write(m,*) ' ?dotest1:  gridherm1:  iwarn=',
     >   iwarn
      if(ier.ne.0) write(m,*) ' ?dotest1:  gridherm1:  ier=',ier
c
      call gridpc1(xt,nt,testa5,ns,xpkg,f,iwarn,ier)
      if(iwarn.ne.0) write(m,*) ' ?dotest1:  gridpc1:  iwarn=',
     >   iwarn
      if(ier.ne.0) write(m,*) ' ?dotest1:  gridpc1:  ier=',ier
c
      fmin=1.0e30
      fmax=-fmin
      sdif=0.0
      hdif=0.0
      pdif=0.0
      s2dif=0.0
      pldif=0.0
      sdifr=0.0
      hdifr=0.0
      pdifr=0.0
      s2difr=0.0
      pldifr=0.0
c
      ier=0
c
      do i=1,nt
         fmin=min(ft(i),fmin)
         fmax=max(ft(i),fmax)
c
         hermv=testa4(i)
cxx         call herm1ev(xt(i),x,ns,ilinx,fherm,ict,hermv,ier)
cxx         if(ier.ne.0) write(m,'('' ?? ier.ne.0 in dotest1 (herm1ev)'')')
c
         difabs=abs(hermv-ft(i))
         hdif=max(hdif,difabs)
         hdifr=max(hdifr,difabs/ft(i))
c
         splinv=testa1(i)
cxx         call cspeval(xt(i),ict,splinv,x,ns,ilinx,fspl,ier)
cxx         if(ier.ne.0) write(m,'('' ?? ier.ne.0 in dotest1 (cspeval)'')')
c
         difabs=abs(splinv-ft(i))
         sdif=max(sdif,difabs)
         sdifr=max(sdifr,difabs/ft(i))
c
         splinv=testa2(i)
cxx         call cspeval(xt(i),ict,splinv,x,ns,ilinx,fspp,ier)
cxx         if(ier.ne.0) write(m,'('' ?? ier.ne.0 in dotest1 (cspeval)'')')
c
         difabs=abs(splinv-ft(i))
         pdif=max(pdif,difabs)
         pdifr=max(pdifr,difabs/ft(i))
c
         splinv=testa3(i)
cxx         call evspline(xt(i),x,ns,ilinx,fs2,ict,splinv,ier)
cxx         if(ier.ne.0) write(m,'('' ?? ier.ne.0 in dotest1 (cspeval)'')')
c
         difabs=abs(splinv-ft(i))
         s2dif=max(s2dif,difabs)
         s2difr=max(s2difr,difabs/ft(i))
c
         zlinv=testa5(i)
         difabs=abs(zlinv-ft(i))
         pldif=max(pldif,difabs)
         pldifr=max(pldifr,difabs/ft(i))
c
      enddo
c
      write(m,1000) '1d periodic spline',ns,fmin,fmax,pdif,pdifr
      write(m,1000) '1d spline w/bdy cond.',ns,fmin,fmax,sdif,sdifr
      write(m,1000) '1d compact spline w/bdy cond.',ns,fmin,fmax,
     >   s2dif,s2difr
      write(m,1000) '1d Hermite interpolation',ns,fmin,fmax,hdif,hdifr
      write(m,1000) '1d piecewise linear',ns,fmin,fmax,pldif,pldifr
 1000 format(/2x,a,' setup on ',i3,' point grid'/
     >   '   fmin=',1pe11.4,' fmax=',1pe11.4,' max(|diff|)=',1pe11.4/
     >   '     max(|diff|/f) = ',1pe11.4)
c
      return
      end
c
c------------------------------------------------
c
      subroutine pspltest2(zctrl)
c
      real, intent(in) :: zctrl
c
      common/pspltest_io/ m
c
c  test various bicubic spline routines
c  for fitting the function
c
c     f(x,th)=exp(2*x-1)*(2+sin(th))
c
c  on 3 grid sizes; compare accuracy on test grid.
c
c  grid ranges:  x in [0,1], th in [0,2pi]
c
      real x1(10),x2(20),x4(40),ex1(10),ex2(20),ex4(40)
      real t1(10),t2(20),t4(40),st1(10),st2(20),st4(40),
     >   ct1(10),ct2(20),ct4(40)
      real bcx1(40),bcx2(40),bcth1(40),bcth2(40)
c
      real f1(4,4,10,10),f2(4,4,20,20),f4(4,4,40,40),fh(6400)
      real flin(1600)
c
      real xtest(200),extest(200),ttest(200),stest(200),ctest(200)
      real zero,one
c
      data pi2/6.2831853/
      data zero,one/0.0,1.0/
c
c---------------------------
c
      call xset(10,x1,ex1,zero,one)
      call xset(20,x2,ex2,zero,one)
      call xset(40,x4,ex4,zero,one)
      call xset(200,xtest,extest,zero,one)
c
      call tset(10,t1,st1,ct1,zero,pi2)
      call tset(20,t2,st2,ct2,zero,pi2)
      call tset(40,t4,st4,ct4,zero,pi2)
      call tset(200,ttest,stest,ctest,zero,pi2)
c
      call ffset(10,ex1,st1,f1)
      call ffset(20,ex2,st2,f2)
      call ffset(40,ex4,st4,f4)
c
      call dotest2(x1,ex1,10,t1,st1,ct1,10,f1,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,
     >   xtest,extest,ttest,stest,200)
c
      call dotest2(x2,ex2,20,t2,st2,ct2,20,f2,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,
     >   xtest,extest,ttest,stest,200)
c
      call dotest2(x4,ex4,40,t4,st4,ct4,40,f4,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,
     >   xtest,extest,ttest,stest,200)
c
      return
      end
c-----------------------------------------------------------
      subroutine xset(nx,x,ex,xmin,xmax)
c
      real x(nx)
      real ex(nx)
c
      do ix=1,nx
         x(ix)=xmin + float(ix-1)*(xmax-xmin)/float(nx-1)
         ex(ix)=exp(2.0*x(ix)-1.0)
      enddo
c
      return
      end
c-----------------------------------------------------------
      subroutine tset(nth,th,sth,cth,thmin,thmax)
c
      real th(nth)
      real sth(nth)
      real cth(nth)
c
      do ith=1,nth
         th(ith)=thmin + float(ith-1)*(thmax-thmin)/float(nth-1)
         sth(ith)=2.0+sin(th(ith))
         cth(ith)=cos(th(ith))
      enddo
c
      return
      end
c-----------------------------------------------------------
      subroutine tset3(nph,ph,sph,cph,phmin,phmax)
c
      real ph(nph)
      real sph(nph)
      real cph(nph)
c
      do iph=1,nph
         ph(iph)=phmin + float(iph-1)*(phmax-phmin)/float(nph-1)
         sph(iph)=3.0+sin(ph(iph))
         cph(iph)=cos(ph(iph))
      enddo
c
      return
      end
c-----------------------------------------------------------
      subroutine ffset(num,xf,tf,f)
c
      real xf(num),tf(num),f(4,4,num,num)
c
      do j=1,num
         do i=1,num
            f(1,1,i,j)=xf(i)*tf(j)
         enddo
      enddo
c
      return
      end
c-----------------------------------------------------------
      subroutine fset3(num,xf,tf,pf,f)
c
      real xf(num),tf(num),pf(num),f(4,4,4,num,num,num)
c
      do k=1,num
         do j=1,num
            do i=1,num
               f(1,1,1,i,j,k)=xf(i)*tf(j)*pf(k)
            enddo
         enddo
      enddo
c
      return
      end
c----------------------------------------------------------
      subroutine bset(fx,nx,fth,nth,bcx1,bcx2,bcth1,bcth2)
c
      real fx(nx)                       ! x factor of test fcn
      real fth(nth)                     ! th factor of test fcn
      real bcx1(nth),bcx2(nth)          ! df/dx bdyy conds at x(1),x(nx)
      real bcth1(nx),bcth2(nx)          ! df/dth bdy conds at th(1),th(nth)
c
c  df/dx = 2*exp(2x-1)*(2+sin(th)) = 2*f
c
      do ith=1,nth
         bcx1(ith)=2.0*fx(1)*fth(ith)   ! df/dx @ x(1)
         bcx2(ith)=2.0*fx(nx)*fth(ith)  ! df/dx @ x(nx)
      enddo
c
c  df/dth = exp(2x-1)*cos(th)
c  cos(0)=cos(2pi)=1
c
      do ix=1,nx
         bcth1(ix)=fx(ix)               ! df/dth @ th=0 = th(1)
         bcth2(ix)=fx(ix)               ! df/dth @ th=2pi = th(nth)
      enddo
c
      return
      end
c---------------------------------------------------------
      subroutine dotest2(x,fx,nx,th,fth,dfth,nth,f,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,
     >   xtest,fxtest,thtest,fthtest,ntest)
c
      use pspline_calls
c
      common/pspltest_io/ m
c
c  test spline of f; f(i,j)=fx(i)*fth(j) on spline grid
c                    f(i,j)=fxtest(i)*fthtest(j) on test grid
c
c  f is exp(2x-1)*(2+sin(th))  df/dx = 2*f
c
      real x(nx),fx(nx)                 ! x & fx vectors (already set)
      real th(nth),fth(nth),dfth(nth)   ! th & fth already set
c
      real f(4,4,nx,nth)                ! spline, f(1,1,*,*) already set
      real fh(0:3,nx,nth)               ! hermite array
      real flin(nx,nth)                 ! piecewise linear array
c
      real bcx1(nth),bcx2(nth)          ! bcs: dfdx vs. th @ x(1), x(nx).
      real bcth1(nx),bcth2(nx)          ! bcs: dfdth vs. x @ th(1), th(nth).
c
      real xtest(ntest),fxtest(ntest)   ! x test grid & fx
      real thtest(ntest),fthtest(ntest) ! th test grid & fth
c
      real wk(64000)                    ! workspace
c
      common/bc_ctrl/ nbc
c
c--------------
c
      if(ntest*ntest.gt.64000) then
         write(m,*) ' ?dotest2:  ntest*ntest exceeds workspace dim.'
         return
      endif
c
c  set up hermite array
c
      do ith=1,nth
         do ix=1,nx
            flin(ix,ith)=f(1,1,ix,ith)         ! f
            fh(0,ix,ith)=f(1,1,ix,ith)         ! f
            fh(1,ix,ith)=2.0*f(1,1,ix,ith)     ! df/dx
            fh(2,ix,ith)=fx(ix)*dfth(ith)      ! df/dy
            fh(3,ix,ith)=2.0*fx(ix)*dfth(ith)  ! d2f/dxdy
         enddo
      enddo
c
      call akherm2p(x,nx,th,nth,fh,nx,ilinx,ilinth,2,1,ier)
c
      call compare('hermite',x,nx,th,nth,f,fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
c  set bdy conds
c
      call bset(fx,nx,fth,nth,bcx1,bcx2,bcth1,bcth2)
c
      call bpspline(x,nx,th,nth,f,nx,wk,15000,ilinx,ilinth,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest2(bpspline)'
      endif
c
      call compare('bpspline',x,nx,th,nth,f,fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
c
      call bpsplinb(x,nx,th,nth,f,nx,
     >   nbc,bcx1,nbc,bcx2,
     >   wk,15000,ilinx,ilinth,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest2(bpsplinb)'
      endif
c
      call compare('bpsplinb',x,nx,th,nth,f,fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
c
      call bcspline(x,nx,th,nth,f,nx,
     >   nbc,bcx1,nbc,bcx2,
     >   nbc,bcth1,nbc,bcth2,
     >   wk,15000,ilinx,ilinth,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest2(bcspline)'
         return
      endif
c
      call compare('bcspline',x,nx,th,nth,f,fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
c  compact spline representation (as per L. Zakharov)
c
      do ith=1,nth
         do ix=1,nx
            fh(0,ix,ith)=f(1,1,ix,ith)         ! f
         enddo
      enddo
c
      call mkbicub(x,nx,th,nth,fh,nx,
     >   nbc,bcx1,nbc,bcx2,
     >   nbc,bcth1,nbc,bcth2,
     >   ilinx,ilinth,ier)
c
      call compare('mkbicub',x,nx,th,nth,f,fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
c  piecewise linear...
c
      call compare('piecewise linear',x,nx,th,nth,f,
     >   fh,flin,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
      return
      end
c--------------------------------
c
      subroutine compare(slbl,x,nx,th,nth,f,fh,fl,ilinx,ilinth,
     >   xtest,fxtest,thtest,fthtest,ntest,wk)
c
      use pspline_calls
c
      common/pspltest_io/ m
c
      character*(*) slbl                ! spline coeff routine:  label
      real x(nx),th(nth)                ! indep. coords.
      real f(4,4,nx,nth)                ! spline data
      real fh(0:3,nx,nth)               ! hermite data
      real fl(nx,nth)                   ! piecewise linear data
      integer ilinx,ilinth              ! even spacing flags
c
c  test data grid & data:
c
      real xtest(ntest),fxtest(ntest),thtest(ntest),fthtest(ntest)
      real wk(ntest,ntest)
c
c-------------
c  select spline fcn eval only (no derivatives)
c
      integer isel(10)
      data isel/1,0,0,0,0,0,0,0,0,0/
c
      real fget(10)
c
      real xpkg(200,1),thpkg(200,1)
c-------------
c
      icycle=20
c
      iherm=0
      if(slbl.eq.'hermite') then
         write(m,*) ' '
         iherm=1
      endif
      if(slbl.eq.'mkbicub') then
         iherm=2
      endif
      if(slbl.eq.'piecewise linear') then
         iherm=3
      endif
c
      fmin=1.0e30
      fmax=-1.0e30
      fdif=0.0
      fdifr=0.0
c
      call genxpkg(nx,x,xpkg,0,1,0,zdum,1,ier)
      call genxpkg(nth,th,thpkg,1,1,0,zdum,1,ier)
c
      if(iherm.eq.0) then
         call cptimer(ztime1)
         do ijk=1,icycle
            call bcspgrid(xtest,ntest,thtest,ntest,wk,ntest,
     >         nx,xpkg,nth,thpkg,f,nx,iwarn,ier)
         enddo
         call cptimer(ztime2)
      else if(iherm.eq.1) then
         call cptimer(ztime1)
         do ijk=1,icycle
            call gridherm2(xtest,ntest,thtest,ntest,wk,ntest,
     >         nx,xpkg,nth,thpkg,fh,nx,iwarn,ier)
         enddo
         call cptimer(ztime2)
      else if(iherm.eq.2) then
         call cptimer(ztime1)
         do ijk=1,icycle
            call gridbicub(xtest,ntest,thtest,ntest,wk,ntest,
     >         nx,xpkg,nth,thpkg,fh,nx,iwarn,ier)
         enddo
         call cptimer(ztime2)
      else
         call cptimer(ztime1)
         do ijk=1,icycle
            call gridpc2(xtest,ntest,thtest,ntest,wk,ntest,
     >         nx,xpkg,nth,thpkg,fl,nx,iwarn,ier)
         enddo
         call cptimer(ztime2)
      endif
c
      do j=1,ntest
         zth=thtest(j)
         do i=1,ntest
            zx=xtest(i)
c
            ff=fxtest(i)*fthtest(j)
            fmin=min(fmin,ff)
            fmax=max(fmax,ff)
c
            if(iherm.eq.0) then
               fget(1)=wk(i,j)
cxx               call bcspeval(zx,zth,
cxx     >            isel,fget,x,nx,th,nth,ilinx,ilinth,
cxx     >            f,nx,ier)
            else if(iherm.eq.1) then
               fget(1)=wk(i,j)
cxx               call herm2ev(zx,zth,x,nx,th,nth,ilinx,ilinth,
cxx     >            f,nx,isel,fget,ier)
            else if(iherm.eq.2) then
               fget(1)=wk(i,j)
cxx               call evbicub(zx,zth,x,nx,th,nth,ilinx,ilinth,
cxx     >            f,nx,isel,fget,ier)
            else
               fget(1)=wk(i,j)
            endif
cxx            if(ier.ne.0) then
cxx               write(m,*) ' ??compare ('//slbl//') ier.ne.0 exit'
cxx               return
cxx            endif
c
            fs=fget(1)
c
            fdif=max(fdif,abs(ff-fs))
            fdifr=max(fdifr,abs(ff-fs)/(0.5*(ff+fs)))
            wk(i,j)=ff-fs
c
         enddo
      enddo
c
      zctime=ztime2-ztime1
      write(m,1000) slbl,nx,nth,fmin,fmax,fdif,fdifr
 1000 format(2x,a,' setup on ',i3,' x ',i3,' grid'/
     >   '   fmin=',1pe11.4,' fmax=',1pe11.4,' max(|diff|)=',1pe11.4/
     >   '     max(|diff|/f) = ',1pe11.4)
      write(m,1001) icycle,ntest,ntest,zctime
 1001 format(2x,i3,' x ',i3,' x ',i3,' evaluations, cpu = ',
     >   1pe11.4,' (s)')
c
      return
      end
c------------------------
      subroutine pspltest3(zctrl)
c
      real, intent(in) :: zctrl
c
      common/pspltest_io/ m
c
c  test various bicubic spline routines
c  for fitting the function
c
c     f(x,th,ph)=exp(2*x-1)*(2+sin(th))*(3+sin(ph))
c
c  on 3 grid sizes; compare accuracy on test grid.
c
c  grid ranges:  x in [0,1], th in [0,2pi], ph in [0,2pi].
c
      real x1(10),x2(20),x4(40),ex1(10),ex2(20),ex4(40)
      real t1(10),t2(20),t4(40),st1(10),st2(20),st4(40),
     >   ct1(10),ct2(20),ct4(40)
      real p1(10),p2(20),p4(40),sp1(10),sp2(20),sp4(40),
     >   cp1(10),cp2(20),cp4(40)
      real bcx1(1600),bcx2(1600)
      real bcth1(1600),bcth2(1600)
      real bcph1(1600),bcph2(1600)
c
      real f1(4,4,4,10,10,10),f2(4,4,4,20,20,20),f4(4,4,4,40,40,40)
      real fh(8,40,40,40),flin(40,40,40)
c
      real xtest(100),extest(100),ttest(100),stest(100),zdum(100)
      real phtest(100),sptest(100)
      real zero,one
c
      data pi2/6.2831853/
      data zero,one/0.0,1.0/
c
c---------------------------
c
      call xset(10,x1,ex1,zero,one)
      call xset(20,x2,ex2,zero,one)
      call xset(40,x4,ex4,zero,one)
      call xset(100,xtest,extest,zero,one)
c
      call tset(10,t1,st1,ct1,zero,pi2)
      call tset(20,t2,st2,ct2,zero,pi2)
      call tset(40,t4,st4,ct4,zero,pi2)
      call tset(100,ttest,stest,zdum,zero,pi2)
c
      call tset3(10,p1,sp1,cp1,zero,pi2)
      call tset3(20,p2,sp2,cp2,zero,pi2)
      call tset3(40,p4,sp4,cp4,zero,pi2)
      call tset3(100,phtest,sptest,zdum,zero,pi2)
c
      call fset3(10,ex1,st1,sp1,f1)
      call fset3(20,ex2,st2,sp2,f2)
      call fset3(40,ex4,st4,sp4,f4)
c
      call dotest3(x1,ex1,10,t1,st1,ct1,10,p1,sp1,cp1,10,f1,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2,
     >   xtest,extest,ttest,stest,phtest,sptest,100)
c
      call dotest3(x2,ex2,20,t2,st2,ct2,20,p2,sp2,cp2,20,f2,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2,
     >   xtest,extest,ttest,stest,phtest,sptest,100)
c
      call dotest3(x4,ex4,40,t4,st4,ct4,40,p4,sp4,cp4,40,f4,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2,
     >   xtest,extest,ttest,stest,phtest,sptest,100)
c
      return
      end
c---------------------------------------------------------
      subroutine dotest3(x,fx,nx,th,fth,dfth,nth,ph,fph,dfph,nph,
     >   f,fh,flin,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      use pspline_calls
c
      common/pspltest_io/ m
c
c  test spline of f; f(i,j,k)=fx(i)*fth(j)*fph(k) on spline grid
c                    f(i,j,k)=fxtest(i)*fthtest(j)*fphtest(k) on test grid
c
c  f is exp(2x-1)*(2+sin(th))*(3+sin(ph)) -- derivatives for BCs evaluated
c  here...
c
c  df/dx = 2*f
c
      real x(nx),fx(nx)                 ! x & fx vectors (already set)
      real th(nth),fth(nth),dfth(nth)   ! th & fth & fth' already set
      real ph(nph),fph(nph),dfph(nth)   ! ph & fph & fph' already set
c
      real f(4,4,4,nx,nth,nph)          ! spline, f(1,1,1,*,*,*) already set
      real fh(0:7,nx,nth,nph)           ! hermite array
      real flin(nx,nth,nph)             ! function data only -- array
c
      real bcx1(nth,nph),bcx2(nth,nph)  ! bcs: dfdx vs. th,ph @ x(1), x(nx).
      real bcth1(nx,nph),bcth2(nx,nph)  ! bcs: dfdth vs. x,ph @ th(1), th(nth).
      real bcph1(nx,nth),bcph2(nx,nth)  ! bcs: dfdph vs. th,ph @ ph(1), ph(nph)
c
      real xtest(ntest),fxtest(ntest)   ! x test grid & fx
      real thtest(ntest),fthtest(ntest) ! th test grid & fth
      real phtest(ntest),fphtest(ntest) ! ph test grid & fph
c
      real wk(80*40*40*40)
c
      common/bc_ctrl/ nbc
c
c--------------
      real zsave(20,20,20)
      real zvals(10)
      integer iselect(10)
      data iselect/1,1,1,1,0,0,0,0,0,0/
c--------------
c
      inwk=80*40*40*40
c
      itot=ntest*ntest*ntest
      write(m,999) itot
 999  format(/
     >   ' %dotest3:  4 x ',i7,
     >   ' evaluations in progress -- be patient.'/)
c
      do iph=1,nph
         do ith=1,nth
            do ix=1,nx
               flin(ix,ith,iph)=f(1,1,1,ix,ith,iph)
               fh(0,ix,ith,iph)=f(1,1,1,ix,ith,iph)
               fh(1,ix,ith,iph)=2.0*f(1,1,1,ix,ith,iph)
               fh(2,ix,ith,iph)=fx(ix)*dfth(ith)*fph(iph)
               fh(3,ix,ith,iph)=fx(ix)*fth(ith)*dfph(iph)
               fh(4,ix,ith,iph)=2.0*fx(ix)*dfth(ith)*fph(iph)
               fh(5,ix,ith,iph)=2.0*fx(ix)*fth(ith)*dfph(iph)
               fh(6,ix,ith,iph)=fx(ix)*dfth(ith)*dfph(iph)
               fh(7,ix,ith,iph)=2.0*fx(ix)*dfth(ith)*dfph(iph)
            enddo
         enddo
      enddo
c
      call akherm3p(x,nx,th,nth,ph,nph,fh,nx,nth,
     >   ilinx,ilinth,ilinph,2,1,1,ier)
c
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest3(akherm3p)'
      endif
c
      call compare3('hermite',x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
c  set bdy conds
c
      call bset3(fx,nx,fth,nth,fph,nph,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2)
c
      call tpspline(x,nx,th,nth,ph,nph,f,nx,nth,
     >   wk,inwk,ilinx,ilinth,ilinph,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest3(tpspline)'
      endif
c
      call compare3('tpspline',x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
c
      call tpsplinb(x,nx,th,nth,ph,nph,f,nx,nth,
     >   nbc,bcx1,nbc,bcx2,nth,
     >   wk,inwk,ilinx,ilinth,ilinph,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest3(tpsplinb)'
      endif
c
      call compare3('tpsplinb',x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      if(max(nx,nth,nph).le.20) then
         do iph=1,nph
            do ith=1,nth
               do ix=1,nx
                  zsave(ix,ith,iph)=f(1,1,1,ix,ith,iph)
               enddo
            enddo
         enddo
      endif
c
      call cptimer(ztime1)
      call tcspline(x,nx,th,nth,ph,nph,f,nx,nth,
     >   nbc,bcx1,nbc,bcx2,nth,
     >   nbc,bcth1,nbc,bcth2,nx,
     >   nbc,bcph1,nbc,bcph2,nx,
     >   wk,inwk,ilinx,ilinth,ilinph,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest3(tcspline)'
         return
      endif
      call cptimer(ztime2)
      write(m,7706) nx,ztime2-ztime1
 7706 format(
     >   ' ==> tcspline setup (nx=',i3,') cpu time (s):',1pe11.4,
     >   ' <=================== ')
c
      if(max(nx,nth,nph).le.20) then
         do iph=1,nph
            do ith=1,nth
               do ix=1,nx
                  call tcspeval(x(ix),th(ith),ph(iph),
     >               iselect, zvals,
     >               x,nx,th,nth,ph,nph, 1, 1, 1, f,nx,nth, ier)
                  zdiff=abs(zsave(ix,ith,iph)-zvals(1))
                  zdiffr=zdiff/zvals(1)
                  if(zdiffr.gt.(1.0e-6)) then
                     write(m,7701) ix,ith,iph,zsave(ix,ith,iph),zvals(1)
                  endif
               enddo
            enddo
         enddo
c
 7701    format(' ix=',i2,' ith=',i2,' iph=',i2,' ** f changed:',
     >      2(1x,1pe13.6))
c
         do iph=1,nph
            do ith=1,nth
               do ix=1,nx,nx-1
                  if(ix.eq.1) then
                     zbc=bcx1(ith,iph)
                  else
                     zbc=bcx2(ith,iph)
                  endif
                  call tcspeval(x(ix),th(ith),ph(iph), iselect, zvals,
     >               x,nx,th,nth,ph,nph, 1, 1, 1, f,nx,nth, ier)
                  if(nbc.eq.1) then
                     if(abs(zbc-zvals(2)).gt.1.0e-4)
     >                  write(m,7702) ix,ith,iph,zbc,zvals(2)
                  endif
               enddo
            enddo
         enddo
c
 7702    format(' df/dx BC check @ix=',i2,',ith=',i2,',iph=',i2,': ',
     >      2(1x,1pe13.6))
c
         do iph=1,nph
            do ix=1,nx
               do ith=1,nth,nth-1
                  if(ith.eq.1) then
                     zbc=bcth1(ix,iph)
                  else
                     zbc=bcth2(ix,iph)
                  endif
                  call tcspeval(x(ix),th(ith),ph(iph), iselect, zvals,
     >               x,nx,th,nth,ph,nph, 1, 1, 1, f,nx,nth, ier)
                  if(nbc.eq.1) then
                     if(abs(zbc-zvals(3)).gt.1.0e-4)
     >                  write(m,7703) ix,ith,iph,zbc,zvals(3)
                  endif
               enddo
            enddo
         enddo
c
 7703    format(' df/dth BC check @ix=',i2,',ith=',i2,',iph=',i2,': ',
     >      2(1x,1pe13.6))
c
         do iph=1,nph,nph-1
            do ith=1,nth
               do ix=1,nx
                  if(iph.eq.1) then
                     zbc=bcph1(ix,ith)
                  else
                     zbc=bcph2(ix,ith)
                  endif
                  call tcspeval(x(ix),th(ith),ph(iph), iselect, zvals,
     >               x,nx,th,nth,ph,nph, 1, 1, 1, f,nx,nth, ier)
                  if(nbc.eq.1) then
                     if(abs(zbc-zvals(4)).gt.1.0e-4)
     >                  write(m,7704) ix,ith,iph,zbc,zvals(4)
                  endif
               enddo
            enddo
         enddo
c
 7704    format(' df/dph BC check @ix=',i2,',ith=',i2,',iph=',i2,': ',
     >      2(1x,1pe13.6))
c
      endif
c
      call compare3('tcspline',x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      do iph=1,nph
         do ith=1,nth
            do ix=1,nx
               fh(0,ix,ith,iph)=f(1,1,1,ix,ith,iph)
            enddo
         enddo
      enddo
c
      call cptimer(ztime1)
      call mktricub(x,nx,th,nth,ph,nph,fh,nx,nth,
     >   nbc,bcx1,nbc,bcx2,nth,
     >   nbc,bcth1,nbc,bcth2,nx,
     >   nbc,bcph1,nbc,bcph2,nx,
     >   ilinx,ilinth,ilinph,ier)
      if(ier.ne.0) then
         write(m,*) ' ?? error in pspltest:  dotest3(mktricub)'
         return
      endif
      call cptimer(ztime2)
      write(m,7707) nx,ztime2-ztime1
 7707 format(
     >   ' ==> mktricub setup (nx=',i3,') cpu time (s):',1pe11.4,
     >   ' <=================== ')
c
      call compare3('mktricub',
     >   x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      call compare3('piecewise linear',
     >   x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      return
      end
c----------------------------------------------------------
      subroutine bset3(fx,nx,fth,nth,fph,nph,
     >   bcx1,bcx2,bcth1,bcth2,bcph1,bcph2)
c
      real fx(nx)                       ! x factor of test fcn
      real fth(nth)                     ! th factor of test fcn
      real fph(nph)                     ! ph factor of test fcn
      real bcx1(nth,nph),bcx2(nth,nph)  ! df/dx bdy conds at x(1),x(nx)
      real bcth1(nx,nph),bcth2(nx,nph)  ! df/dth bdy conds at th(1),th(nth)
      real bcph1(nx,nth),bcph2(nx,nth)  ! df/dph bdy conds at ph(1),ph(nph)
c
c  df/dx = 2*exp(2x-1)*(2+sin(th))*(3+sin(ph)) = 2*f
c
      do iph=1,nph
         do ith=1,nth
            bcx1(ith,iph)=2.0*fx(1)*fth(ith)*fph(iph) ! df/dx @ x(1)
            bcx2(ith,iph)=2.0*fx(nx)*fth(ith)*fph(iph) ! df/dx @ x(nx)
         enddo
      enddo
c
c  df/dth = exp(2x-1)*cos(th)*(3+sin(ph))
c  cos(0)=cos(2pi)=1
c
      do iph=1,nph
         do ix=1,nx
            bcth1(ix,iph)=fx(ix)*fph(iph)    ! df/dth @ th=0 = th(1)
            bcth2(ix,iph)=fx(ix)*fph(iph)    ! df/dth @ th=2pi = th(nth)
         enddo
      enddo
c
c  df/dph = exp(2x-1)*(2+sin(th))*cos(ph)
c  cos(0)=cos(2pi)=1
c
      do ith=1,nth
         do ix=1,nx
            bcph1(ix,ith)=fx(ix)*fth(ith)    ! df/dph @ ph=0
            bcph2(ix,ith)=fx(ix)*fth(ith)    ! df/dph @ ph=2pi
         enddo
      enddo
c
      return
      end
c--------------------------------
c
      subroutine compare3(slbl,x,nx,th,nth,ph,nph,f,fh,flin,
     >   ilinx,ilinth,ilinph,
     >   xtest,fxtest,thtest,fthtest,phtest,fphtest,ntest)
c
      use pspline_calls
c
      common/pspltest_io/ m
c
      character*(*) slbl                ! spline coeff routine:  label
      real x(nx),th(nth),ph(nph)        ! indep. coords.
      real f(4,4,4,nx,nth,nph)          ! spline data
      real fh(0:7,nx,nth,nph)           ! hermite data
      real flin(nx,nth,nph)             ! pc linear data
      integer ilinx,ilinth,ilinph       ! even spacing flags
c
c  test data grid & data:
c
      real xtest(ntest),fxtest(ntest)
      real thtest(ntest),fthtest(ntest)
      real phtest(ntest),fphtest(ntest)
c
c-------------
      real xpkg(ntest,4),thpkg(ntest,4),phpkg(ntest,4)
      real thvec(ntest),phvec(ntest),fvec(ntest,1)
c-------------
c  select spline fcn eval only (no derivatives)
c
      integer isel(10)
      data isel/1,0,0,0,0,0,0,0,0,0/
c
      real fget(10)
c
c-------------
c
      iherm=0
      if(slbl.eq.'hermite') iherm=1
      if(slbl.eq.'mktricub') iherm=2
      if(slbl.eq.'piecewise linear') iherm=3
c
      fmin=1.0e30
      fmax=-1.0e30
      fdif=0.0
      fdifr=0.0
c
      call genxpkg(nx,x,xpkg,0,1,0,zdum,1,ier)
      call genxpkg(nth,th,thpkg,1,1,0,zdum,1,ier)
      call genxpkg(nph,ph,phpkg,1,1,0,zdum,1,ier)
c
      call cptimer(ztime1)
      do k=1,ntest
         zph=phtest(k)
         do j=1,ntest
            phvec(j)=zph
         enddo
         do j=1,ntest
            zth=thtest(j)
            do i=1,ntest
               thvec(i)=zth
            enddo
            if(iherm.eq.0) then
               call tcspvec(isel,ntest,xtest,thvec,phvec,
     >            ntest,fvec,nx,xpkg,nth,thpkg,nph,phpkg,
     >            f,nx,nth,iwarn,ier)
            else if(iherm.eq.1) then
               call vecherm3(isel,ntest,xtest,thvec,phvec,
     >            ntest,fvec,nx,xpkg,nth,thpkg,nph,phpkg,
     >            fh,nx,nth,iwarn,ier)
            else if(iherm.eq.2) then
               call vectricub(isel,ntest,xtest,thvec,phvec,
     >            ntest,fvec,nx,xpkg,nth,thpkg,nph,phpkg,
     >            fh,nx,nth,iwarn,ier)
            else if(iherm.eq.3) then
               call vecpc3(isel,ntest,xtest,thvec,phvec,
     >            ntest,fvec,nx,xpkg,nth,thpkg,nph,phpkg,
     >            flin,nx,nth,iwarn,ier)
            endif
            do i=1,ntest
               zx=xtest(i)
c
               ff=fxtest(i)*fthtest(j)*fphtest(k)
               fmin=min(fmin,ff)
               fmax=max(fmax,ff)
c
               if(iherm.eq.0) then
cxx                  call tcspeval(zx,zth,zph,
cxx     >               isel,fget,x,nx,th,nth,ph,nph,
cxx     >               ilinx,ilinth,ilinph,
cxx     >               f,nx,nth,ier)
                  fget(1)=fvec(i,1)
               else if(iherm.eq.1) then
cxx                  call herm3ev(zx,zth,zph,x,nx,th,nth,ph,nph,
cxx     >               ilinx,ilinth,ilinph,
cxx     >               fh,nx,nth,isel,fget,ier)
                  fget(1)=fvec(i,1)
               else if(iherm.eq.2) then
cxx                  call evtricub(zx,zth,zph,x,nx,th,nth,ph,nph,
cxx     >               ilinx,ilinth,ilinph,
cxx     >               fh,nx,nth,isel,fget,ier)
                  fget(1)=fvec(i,1)
               else
                  fget(1)=fvec(i,1)
               endif
cxx               if(ier.ne.0) then
cxx                  write(m,*) ' ??compare3 ('//slbl//') ier.ne.0 exit'
cxx                  return
cxx               endif
c
               fs=fget(1)
c
               fdif=max(fdif,abs(ff-fs))
               fdifr=max(fdifr,abs(ff-fs)/(0.5*(ff+fs)))
c
            enddo
         enddo
      enddo
      call cptimer(ztime2)
c
      write(m,1000) slbl,nx,nth,nph,fmin,fmax,fdif,fdifr
 1000 format(2x,a,' setup on ',i3,' x ',i3,' x ',i3,' grid'/
     >   '   fmin=',1pe11.4,' fmax=',1pe11.4,' max(|diff|)=',1pe11.4/
     >   '     max(|diff|/f) = ',1pe11.4)
c
      zctime=ztime2-ztime1
      write(m,1001) ntest,ntest,ntest,zctime
 1001 format('  ',i3,' x ',i3,' x ',i3,' evaluations, cpu = ',
     >   1pe11.4,' (s)')
c
      return
      end
