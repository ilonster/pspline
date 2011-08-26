program qk_pspline

  !  ad hoc test program for pspline -- evaluate all fcn values and
  !  derivatives via ezspline and f77 traditional interfaces

  use ezspline_obj
  use ezspline
  implicit NONE

  integer, parameter :: nx=51, ny=61,nz=31
  INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)

  real(r8) :: x(nx),y(ny),z(nz),f(nx),ff(nx,ny),fff(nx,ny,nz)
  real :: fr4(nx),ffr4(nx,ny),fffr4(nx,ny,nz)
  real(r8) :: xpkg(nx,4),ypkg(ny,4),zpkg(nz,4),zwk(16*nx*ny*nz)

  real(r8) :: fspl(4,nx),ffspl(4,4,nx,ny),fffspl(4,4,4,nx,ny,nz)

  real(r8), parameter :: c2pi = 6.2831853071795862_R8

  integer :: iper,ialg,itol,imsg

  integer :: ix,iy,iz,ier,iwarn,iwksize,idum,ilinx,iliny,ilinz
  real(r8) :: xr,yr,ztol,zdum

  type(ezspline1_r8) :: s1
  type(ezspline2_r8) :: s2,h20,h02
  type(ezspline3_r8) :: s3,h220,h202,h022

  type(ezspline1_r4) :: t1
  type(ezspline2_r4) :: t2
  type(ezspline3_r4) :: t3

  integer :: ibdy(2)

  !---------------------------------------------------------------------
  !  test data -- choose zones & offsets for tests
  !    if offsets are too close to a grid boundary, then finite-difference
  !    evaluation comparisons will fail for 3rd derivatives which are not
  !    continuous.
  
  integer, parameter :: ixtest = 36  ! avoid bdy zones -- continuity test...
  integer, parameter :: iytest = 15  ! avoid bdy zones -- continuity test...
  integer, parameter :: iztest = 12  ! avoid bdy zones -- continuity test...

  real*8, parameter :: xoff = 0.9_r8
  real*8, parameter :: yoff = 0.2_r8
  real*8, parameter :: zoff = 0.5_r8

  real*8 :: xtarg,ytarg,ztarg,xbrak(6),ybrak(6),zbrak(6),delx,dely,delz
  real :: xtargr4,ytargr4,ztargr4,xbrakr4(6),ybrakr4(6),zbrakr4(6)
  real*8 :: zval,zvec(6),zvalp
  real :: zvalr4,zvecr4(6)

  integer :: i1,i2,i3,itest
  integer :: ict(10)
  character*60 zlbl0,zlbl(3)
  character*8 zhdr

  real*8 :: zsum,zorig,zdiff
  real*8,parameter :: zsumtol8 = 1.0d-4
  real*8,parameter :: zsumtol4 = 1.0d-3
  real*8,parameter :: epslon = 1.0d-34

  !---------------------------------------------------------------------
  !  continuity test for values and derivatives...

  real*8 :: xctest(9),yctest(9),zctest(9),fctest(9)
  real*8 :: xconst(9),yconst(9),zconst(9)

  real*8 :: ccbld(4) = (/ 0.05_r8, 0.35_r8, 0.65_r8, 0.95_r8 /)

  character*12 :: cresult
  logical :: cflag,ccflag

  !---------------------------------------------------------------------
  !  initialization of spline objects

  call ezspline_preInit(s1)
  call ezspline_preInit(s2)
  call ezspline_preInit(s3)

  call ezspline_preInit(h20)
  call ezspline_preInit(h02)

  call ezspline_preInit(h220)
  call ezspline_preInit(h202)
  call ezspline_preInit(h022)

  call ezspline_preInit(t1)
  call ezspline_preInit(t2)
  call ezspline_preInit(t3)

  ibdy=0  ! non-a-knot boundary conditions

  call ezspline_init(s1,nx,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezspline_init(s2,nx,ny,ibdy,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezspline_init(s3,nx,ny,nz,ibdy,ibdy,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezspline_init(t1,nx,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezspline_init(t2,nx,ny,ibdy,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezspline_init(t3,nx,ny,nz,ibdy,ibdy,ibdy,ier)
  if(ier.ne.0) call bad_exit

  call ezhybrid_init(h02,nx,ny,(/0,2/), ier)  ! linear-spline
  if(ier.ne.0) call bad_exit

  call ezhybrid_init(h20,nx,ny,(/2,0/), ier)  ! spline-linear
  if(ier.ne.0) call bad_exit

  call ezhybrid_init(h022,nx,ny,nz,(/0,2,2/), ier)  ! linear-spline-spline
  if(ier.ne.0) call bad_exit

  call ezhybrid_init(h202,nx,ny,nz,(/2,0,2/), ier)  ! spline-linear-spline
  if(ier.ne.0) call bad_exit

  call ezhybrid_init(h220,nx,ny,nz,(/2,2,0/), ier)  ! spline-spline-linear
  if(ier.ne.0) call bad_exit

  !---------------------------------------------------------------------
  !  make grids, define fcns, make explicit splines

  iper=0  ! treat functions as non-periodic
  imsg=1  ! warn on out of bounds interpolation calls
  itol=1  ! set tolerance for even spacing & out of range errors...
  ztol=1.0e-9_R8
  idum=0
  zdum=0
  iwksize=16*nx*ny*nz

  !  make grids & associated lookup packages...

  call makex(x,nx)
  ialg=-1
  call r8genxpkg(nx,x,xpkg,iper,imsg,itol,ztol,ialg,ier)
  if(ier.ne.0) call bad_exit

  s1%x1 = x
  s2%x1 = x
  s3%x1 = x

  t1%x1 = x
  t2%x1 = x
  t3%x1 = x

  h02%x1 = x
  h20%x1 = x

  h022%x1 = x
  h202%x1 = x
  h220%x1 = x

  call makex(y,ny)
  ialg=-2
  call r8genxpkg(ny,y,ypkg,iper,imsg,itol,ztol,ialg,ier)
  if(ier.ne.0) call bad_exit

  s2%x2 = y
  s3%x2 = y

  t2%x2 = y
  t3%x2 = y

  h02%x2 = y
  h20%x2 = y

  h022%x2 = y
  h202%x2 = y
  h220%x2 = y

  call makex(z,nz)
  ialg=3
  call r8genxpkg(nz,z,zpkg,iper,imsg,itol,ztol,ialg,ier)
  if(ier.ne.0) call bad_exit

  s3%x3 = z

  t3%x3 = z

  h022%x3 = z
  h202%x3 = z
  h220%x3 = z

  !----------------------------------------------------------------
  !  make target evaluation points

  xtarg = x(ixtest)+xoff*(x(ixtest+1)-x(ixtest))
  ytarg = y(iytest)+yoff*(y(iytest+1)-y(iytest))
  ztarg = z(iztest)+zoff*(z(iztest+1)-z(iztest))

  xtargr4 = xtarg
  ytargr4 = ytarg
  ztargr4 = ztarg

  xconst = xtarg
  yconst = ytarg
  zconst = ztarg

  delx=2*(x(ixtest+1)-x(ixtest))/100
  xbrak(1)=xtarg-delx/2
  xbrak(2)=xtarg+delx/2
  xbrak(3:6)=xtarg

  dely=2*(y(iytest+1)-y(iytest))/100
  ybrak(1:2)=ytarg
  ybrak(3)=ytarg-dely/2
  ybrak(4)=ytarg+dely/2
  ybrak(5:6)=ytarg

  delz=2*(z(iztest+1)-z(iztest))/100
  zbrak(1:4)=ztarg
  zbrak(5)=ztarg-delz/2
  zbrak(6)=ztarg+delz/2

  xbrakr4 = xbrak
  ybrakr4 = ybrak
  zbrakr4 = zbrak

  xctest(5)=x(ixtest)
  do ix=1,4
     xctest(ix)=(1-ccbld(ix))*x(ixtest-1)+ccbld(ix)*x(ixtest)
     xctest(5+ix)=(1-ccbld(ix))*x(ixtest)+ccbld(ix)*x(ixtest+1)
  enddo

  yctest(5)=y(iytest)
  do iy=1,4
     yctest(iy)=(1-ccbld(iy))*y(iytest-1)+ccbld(iy)*y(iytest)
     yctest(5+iy)=(1-ccbld(iy))*y(iytest)+ccbld(iy)*y(iytest+1)
  enddo

  zctest(5)=z(iztest)
  do iz=1,4
     zctest(iz)=(1-ccbld(iz))*z(iztest-1)+ccbld(iz)*z(iztest)
     zctest(5+iz)=(1-ccbld(iz))*z(iztest)+ccbld(iz)*z(iztest+1)
  enddo

  !----------------------------------------------------------------
  !  make fcns; load explicit spline arrays' data elements

  do iz=1,nz
     do iy=1,ny
        do ix=1,nx
           xr=(sqrt(3.0_r8)/2)*x(ix) - (1.0_r8/2)*y(iy)
           yr=(1.0_r8/2)*x(ix) + (sqrt(3.0_r8)/2)*y(iy)
           if(iz.eq.1) then
              if(iy.eq.1) then
                 f(ix)=cos(2*x(ix))
                 fspl(1,ix)=f(ix)
                 fr4(ix)=f(ix)
              endif
              ff(ix,iy)=sin(3*xr)*cos(2*yr)
              ffspl(1,1,ix,iy)=ff(ix,iy)
              ffr4(ix,iy)=ff(ix,iy)
           endif
           fff(ix,iy,iz)=(cos(2*(2*z(iz)-1)*xr))**2*sin(2*(1-2*z(iz))*yr)
           fffspl(1,1,1,ix,iy,iz)=fff(ix,iy,iz)
           fffr4(ix,iy,iz)=fff(ix,iy,iz)
        enddo
     enddo
  enddo

  !  make explicit spline coefficients

  call r8cspline(x,nx,fspl,ibdy(1),zdum,ibdy(2),zdum,zwk,iwksize,ilinx,ier)
  if(ier.ne.0) call bad_exit

  call r8bcspline(x,nx,y,ny,ffspl,nx, &
       ibdy(1),zdum,ibdy(2),zdum, &
       ibdy(1),zdum,ibdy(2),zdum, &
       zwk,iwksize,ilinx,iliny,ier)
  if(ier.ne.0) call bad_exit

  call r8tcspline(x,nx,y,ny,z,nz,fffspl,nx,ny, &
       ibdy(1),zdum,ibdy(2),zdum,idum, &
       ibdy(1),zdum,ibdy(2),zdum,idum, &
       ibdy(1),zdum,ibdy(2),zdum,idum, &
       zwk,iwksize,ilinx,iliny,ilinz,ier)
  if(ier.ne.0) call bad_exit

  !  set up spline objects

  call ezspline_setup(s1,f,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(s2,ff,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(s3,fff,ier); if(ier.ne.0) call bad_exit

  call ezspline_setup(h02,ff,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(h20,ff,ier); if(ier.ne.0) call bad_exit

  call ezspline_setup(h022,fff,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(h202,fff,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(h220,fff,ier); if(ier.ne.0) call bad_exit

  call ezspline_setup(t1,fr4,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(t2,ffr4,ier); if(ier.ne.0) call bad_exit
  call ezspline_setup(t3,fffr4,ier); if(ier.ne.0) call bad_exit

  !---------------------------------------------------------------
  !  1d tests

  i2=0
  i3=0
  do i1=0,3
     zlbl0=' '
     zlbl=' '
     call maklbl(' <-- ',i1,i2,i3,zlbl0)

     itest=0
     if(i1.gt.0) then
        itest=itest+1
        call maklbl(' <-- finite diff. ',i1-1,i2,i3,zlbl(itest))
     endif

     !----------------
     ! 1d r8 ezspline

     itest=0
     write(6,*) ' '
     write(6,*) ' 1d r8 ezspline:'

     !  test continuity across grid cell bdy:
     !  continuity not expected for 3rd derivatives.

     cresult = ' '
     ccflag=.true.

     call ezspline_derivative(s1,i1,9,xctest,fctest,ier)
     if(ier.ne.0) call bad_exit
     call cont_eval(xctest,fctest,cresult,cflag)
     if(i1.lt.3) ccflag = ccflag.and.cflag

     if(.not.ccflag) then
        write(6,*) '...continuity: ',trim(cresult),' ???????????????????'
     else
        write(6,*) '...continuity: ',trim(cresult)
     endif

     call ezspline_derivative(s1,i1,xtarg,zval,ier)
     if(ier.ne.0) call bad_exit
     write(6,1001) ' value: ',zval,trim(zlbl0)
     zorig=zval

     if(i1.gt.0) then
        itest=itest+1
        write(zhdr,'(" test",i1,": ")') itest

        call ezspline_derivative(s1,i1-1,6,xbrak,zvec,ier)
        if(ier.ne.0) call bad_exit
        zdum=(zvec(2)-zvec(1))/delx
        write(6,1001) zhdr,zdum,trim(zlbl(itest))
        zsum=(zorig+zdum)/2
        zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
        if(zdiff.gt.zsumtol8) write(6,*) &
             ' ------> relative difference: ',zdiff,' ********************* '
     endif

     !----------------
     ! 1d non-compact spline test
     
     zvalp = zval

     itest=0
     write(6,*) ' '
     write(6,*) ' 1d r8 non-compact spline:'

     call ezmake_ict1(i1,ict(1:3))
     call r8cspeval(xtarg,ict,zval,x,nx,ilinx,fspl,ier)
     if(ier.ne.0) call bad_exit
     write(6,1001) ' value: ',zval,trim(zlbl0)
     zorig=zval

     if(abs(zval-zvalp)/max(1.0_r8,abs(zval)).gt.1.0d-10) then
        write(6,*) ' +++++++++> variation: ezspline r8, non-compact: ', &
             zvalp,zval
     endif

     if(i1.gt.0) then
        itest=itest+1
        write(zhdr,'(" test",i1,": ")') itest

        call ezmake_ict1(i1-1,ict(1:3))
        call r8spvec(ict,6,xbrak,6,zvec,nx,xpkg,fspl,iwarn,ier)
        if(ier.ne.0) call bad_exit
        zdum=(zvec(2)-zvec(1))/delx
        write(6,1001) zhdr,zdum,trim(zlbl(itest))
        zsum=(zorig+zdum)/2
        zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
        if(zdiff.gt.zsumtol8) write(6,*) &
             ' ------> relative difference: ',zdiff,' ********************* '
     endif

     !----------------
     ! 1d r4 ezspline

     itest=0
     write(6,*) ' '
     write(6,*) ' 1d r4 ezspline:'

     call ezspline_derivative(t1,i1,xtargr4,zvalr4,ier)
     if(ier.ne.0) call bad_exit
     write(6,1001) ' value: ',zvalr4*1.0_R8,trim(zlbl0)
     zorig=zvalr4

     if(i1.gt.0) then
        itest=itest+1
        write(zhdr,'(" test",i1,": ")') itest

        call ezspline_derivative(t1,i1-1,6,xbrakr4,zvecr4,ier)
        if(ier.ne.0) call bad_exit
        zdum=(zvecr4(2)-zvecr4(1))/delx
        write(6,1001) zhdr,zdum,trim(zlbl(itest))
        zsum=(zorig+zdum)/2
        zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
        if(zdiff.gt.zsumtol4) write(6,*) &
             ' ------> relative difference: ',zdiff,' ********************* '
     endif

  enddo

  write(6,*) ' '
  write(6,*) ' ---------------------------- '

  !---------------------------------------------------------------
  !  2d tests

  i3=0
  do i2=0,3
     do i1=0,3
        zlbl0=' '
        zlbl=' '
        call maklbl(' <-- ',i1,i2,i3,zlbl0)

        itest=0

        if(i1.gt.0) then
           itest=itest+1
           call maklbl(' <-- finite diff. ',i1-1,i2,i3,zlbl(itest))
        endif

        if(i2.gt.0) then
           itest=itest+1
           call maklbl(' <-- finite diff. ',i1,i2-1,i3,zlbl(itest))
        endif

        !----------------
        ! 2d r8 hybrid ezspline

        if(i1.le.1) then

           itest=0
           write(6,*) ' '
           write(6,*) ' 2d r8 ezspline (0,2) hybrid:'

           !  test continuity across grid cell bdy:
           !  continuity not expected for 3rd derivatives.

           cresult = ' '
           ccflag=.true.

           call ezspline_derivative(h02,i1,i2,9,xctest,yconst,fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(xctest,fctest,cresult,cflag)
           if(i1.lt.1) ccflag = ccflag.and.cflag

           call ezspline_derivative(h02,i1,i2,9,xconst,yctest,fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(yctest,fctest,cresult,cflag)
           if(i2.lt.3) ccflag = ccflag.and.cflag

           if(.not.ccflag) then
              write(6,*) '...continuity: ',trim(cresult),' ???????????????????'
           else
              write(6,*) '...continuity: ',trim(cresult)
           endif

           call ezspline_derivative(h02,i1,i2,xtarg,ytarg,zval,ier)
           if(ier.ne.0) call bad_exit
           write(6,1001) ' value: ',zval,trim(zlbl0)
           zorig=zval

           if(i1.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(h02,i1-1,i2,6,xbrak,ybrak,zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(2)-zvec(1))/delx
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
           endif

           if(i2.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(h02,i1,i2-1,6,xbrak,ybrak,zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(4)-zvec(3))/dely
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
           endif
        endif

        !----------------
        ! 2d r8 hybrid ezspline

        if(i2.le.1) then

           itest=0
           write(6,*) ' '
           write(6,*) ' 2d r8 ezspline (2,0) hybrid:'

           !  test continuity across grid cell bdy:
           !  continuity not expected for 3rd derivatives.

           cresult = ' '
           ccflag=.true.

           call ezspline_derivative(h20,i1,i2,9,xctest,yconst,fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(xctest,fctest,cresult,cflag)
           if(i1.lt.3) ccflag = ccflag.and.cflag

           call ezspline_derivative(h20,i1,i2,9,xconst,yctest,fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(yctest,fctest,cresult,cflag)
           if(i2.lt.1) ccflag = ccflag.and.cflag

           if(.not.ccflag) then
              write(6,*) '...continuity: ',trim(cresult),' ???????????????????'
           else
              write(6,*) '...continuity: ',trim(cresult)
           endif

           call ezspline_derivative(h20,i1,i2,xtarg,ytarg,zval,ier)
           if(ier.ne.0) call bad_exit
           write(6,1001) ' value: ',zval,trim(zlbl0)
           zorig=zval

           if(i1.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(h20,i1-1,i2,6,xbrak,ybrak,zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(2)-zvec(1))/delx
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
           endif

           if(i2.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(h20,i1,i2-1,6,xbrak,ybrak,zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(4)-zvec(3))/dely
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
           endif
        endif

        !----------------
        ! 2d r8 ezspline

        itest=0
        write(6,*) ' '
        write(6,*) ' 2d r8 ezspline:'

        !  test continuity across grid cell bdy:
        !  continuity not expected for 3rd derivatives.

        cresult = ' '
        ccflag=.true.

        call ezspline_derivative(s2,i1,i2,9,xctest,yconst,fctest,ier)
        if(ier.ne.0) call bad_exit
        call cont_eval(xctest,fctest,cresult,cflag)
        if(i1.lt.3) ccflag = ccflag.and.cflag

        call ezspline_derivative(s2,i1,i2,9,xconst,yctest,fctest,ier)
        if(ier.ne.0) call bad_exit
        call cont_eval(yctest,fctest,cresult,cflag)
        if(i2.lt.3) ccflag = ccflag.and.cflag


        if(.not.ccflag) then
           write(6,*) '...continuity: ',trim(cresult),' ???????????????????'
        else
           write(6,*) '...continuity: ',trim(cresult)
        endif

        call ezspline_derivative(s2,i1,i2,xtarg,ytarg,zval,ier)
        if(ier.ne.0) call bad_exit
        write(6,1001) ' value: ',zval,trim(zlbl0)
        zorig=zval

        if(i1.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezspline_derivative(s2,i1-1,i2,6,xbrak,ybrak,zvec,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvec(2)-zvec(1))/delx
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

        if(i2.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezspline_derivative(s2,i1,i2-1,6,xbrak,ybrak,zvec,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvec(4)-zvec(3))/dely
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

        !----------------
        ! 2d non-compact spline test
     
        zvalp = zval
        itest=0
        write(6,*) ' '
        write(6,*) ' 2d r8 non-compact spline:'

        call ezmake_ict2(i1,i2,ict(1:6))
        call r8bcspeval(xtarg,ytarg,ict,zval,x,nx,y,ny, &
             ilinx,iliny,ffspl,nx,ier)
        if(ier.ne.0) call bad_exit
        write(6,1001) ' value: ',zval,trim(zlbl0)
        zorig=zval

        if(abs(zval-zvalp)/max(1.0_r8,abs(zval)).gt.1.0d-10) then
           write(6,*) ' +++++++++> variation: ezspline r8, non-compact: ', &
                zvalp,zval
        endif

        if(i1.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezmake_ict2(i1-1,i2,ict(1:6))
           call r8bcspvec(ict,6,xbrak,ybrak,6,zvec, &
                nx,xpkg,ny,ypkg,ffspl,nx,iwarn,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvec(2)-zvec(1))/delx
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

        if(i2.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezmake_ict2(i1,i2-1,ict(1:6))
           call r8bcspvec(ict,6,xbrak,ybrak,6,zvec, &
                nx,xpkg,ny,ypkg,ffspl,nx,iwarn,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvec(4)-zvec(3))/dely
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

        !----------------
        ! 2d r4 ezspline

        itest=0
        write(6,*) ' '
        write(6,*) ' 2d r4 ezspline:'

        call ezspline_derivative(t2,i1,i2,xtargr4,ytargr4,zvalr4,ier)
        if(ier.ne.0) call bad_exit
        write(6,1001) ' value: ',zvalr4*1.0_R8,trim(zlbl0)
        zorig=zvalr4

        if(i1.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezspline_derivative(t2,i1-1,i2,6,xbrakr4,ybrakr4,zvecr4,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvecr4(2)-zvecr4(1))/delx
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

        if(i2.gt.0) then
           itest=itest+1
           write(zhdr,'(" test",i1,": ")') itest

           call ezspline_derivative(t2,i1,i2-1,6,xbrakr4,ybrakr4,zvecr4,ier)
           if(ier.ne.0) call bad_exit
           zdum=(zvecr4(4)-zvecr4(3))/dely
           write(6,1001) zhdr,zdum,trim(zlbl(itest))
           zsum=(zorig+zdum)/2
           zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
           if(zdiff.gt.zsumtol4) write(6,*) &
                ' ------> relative difference: ',zdiff,' ******************'
        endif

     enddo
  enddo

  write(6,*) ' '
  write(6,*) ' ---------------------------- '

  !---------------------------------------------------------------
  !  3d tests

  do i3=0,3
     do i2=0,3
        do i1=0,3
           zlbl0=' '
           zlbl=' '
           call maklbl(' <-- ',i1,i2,i3,zlbl0)

           itest=0

           if(i1.gt.0) then
              itest=itest+1
              call maklbl(' <-- finite diff. ',i1-1,i2,i3,zlbl(itest))
           endif

           if(i2.gt.0) then
              itest=itest+1
              call maklbl(' <-- finite diff. ',i1,i2-1,i3,zlbl(itest))
           endif

           if(i3.gt.0) then
              itest=itest+1
              call maklbl(' <-- finite diff. ',i1,i2,i3-1,zlbl(itest))
           endif

           !----------------
           ! 3d r8 hybrid ezspline

           if(i1.le.1) then

              itest=0
              write(6,*) ' '
              write(6,*) ' 3d r8 ezspline (0,2,2) hybrid:'

              !  test continuity across grid cell bdy:
              !  continuity not expected for 3rd derivatives.

              cresult = ' '
              ccflag=.true.

              call ezspline_derivative(h022,i1,i2,i3,9,xctest,yconst,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(xctest,fctest,cresult,cflag)
              if(i1.lt.1) ccflag = ccflag.and.cflag

              call ezspline_derivative(h022,i1,i2,i3,9,xconst,yctest,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(yctest,fctest,cresult,cflag)
              if(i2.lt.3) ccflag = ccflag.and.cflag

              call ezspline_derivative(h022,i1,i2,i3,9,xconst,yconst,zctest, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(zctest,fctest,cresult,cflag)
              if(i3.lt.3) ccflag = ccflag.and.cflag

              if(.not.ccflag) then
                 write(6,*) '...continuity: ',trim(cresult), &
                      ' ???????????????????'
              else
                 write(6,*) '...continuity: ',trim(cresult)
              endif

              call ezspline_derivative(h022,i1,i2,i3,xtarg,ytarg,ztarg,zval, &
                   ier)
              if(ier.ne.0) call bad_exit
              write(6,1001) ' value: ',zval,trim(zlbl0)
              zorig=zval

              if(i1.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h022,i1-1,i2,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(2)-zvec(1))/delx
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i2.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h022,i1,i2-1,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(4)-zvec(3))/dely
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i3.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h022,i1,i2,i3-1,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(6)-zvec(5))/delz
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif
           endif

           !----------------
           ! 3d r8 hybrid ezspline

           if(i2.le.1) then

              itest=0
              write(6,*) ' '
              write(6,*) ' 3d r8 ezspline (2,0,2) hybrid:'

              !  test continuity across grid cell bdy:
              !  continuity not expected for 3rd derivatives.

              cresult = ' '
              ccflag=.true.

              call ezspline_derivative(h202,i1,i2,i3,9,xctest,yconst,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(xctest,fctest,cresult,cflag)
              if(i1.lt.3) ccflag = ccflag.and.cflag

              call ezspline_derivative(h202,i1,i2,i3,9,xconst,yctest,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(yctest,fctest,cresult,cflag)
              if(i2.lt.1) ccflag = ccflag.and.cflag

              call ezspline_derivative(h202,i1,i2,i3,9,xconst,yconst,zctest, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(zctest,fctest,cresult,cflag)
              if(i3.lt.3) ccflag = ccflag.and.cflag

              if(.not.ccflag) then
                 write(6,*) '...continuity: ',trim(cresult), &
                      ' ???????????????????'
              else
                 write(6,*) '...continuity: ',trim(cresult)
              endif

              call ezspline_derivative(h202,i1,i2,i3,xtarg,ytarg,ztarg,zval, &
                   ier)
              if(ier.ne.0) call bad_exit
              write(6,1001) ' value: ',zval,trim(zlbl0)
              zorig=zval

              if(i1.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h202,i1-1,i2,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(2)-zvec(1))/delx
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i2.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h202,i1,i2-1,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(4)-zvec(3))/dely
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i3.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h202,i1,i2,i3-1,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(6)-zvec(5))/delz
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif
           endif

           !----------------
           ! 3d r8 hybrid ezspline

           if(i3.le.1) then

              itest=0
              write(6,*) ' '
              write(6,*) ' 3d r8 ezspline (2,2,0) hybrid:'

              !  test continuity across grid cell bdy:
              !  continuity not expected for 3rd derivatives.

              cresult = ' '
              ccflag=.true.

              call ezspline_derivative(h220,i1,i2,i3,9,xctest,yconst,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(xctest,fctest,cresult,cflag)
              if(i1.lt.3) ccflag = ccflag.and.cflag

              call ezspline_derivative(h220,i1,i2,i3,9,xconst,yctest,zconst, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(yctest,fctest,cresult,cflag)
              if(i2.lt.3) ccflag = ccflag.and.cflag

              call ezspline_derivative(h220,i1,i2,i3,9,xconst,yconst,zctest, &
                   fctest,ier)
              if(ier.ne.0) call bad_exit
              call cont_eval(zctest,fctest,cresult,cflag)
              if(i3.lt.1) ccflag = ccflag.and.cflag

              if(.not.ccflag) then
                 write(6,*) '...continuity: ',trim(cresult), &
                      ' ???????????????????'
              else
                 write(6,*) '...continuity: ',trim(cresult)
              endif

              call ezspline_derivative(h220,i1,i2,i3,xtarg,ytarg,ztarg,zval, &
                   ier)
              if(ier.ne.0) call bad_exit
              write(6,1001) ' value: ',zval,trim(zlbl0)
              zorig=zval

              if(i1.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h220,i1-1,i2,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(2)-zvec(1))/delx
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i2.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h220,i1,i2-1,i3,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(4)-zvec(3))/dely
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif

              if(i3.gt.0) then
                 itest=itest+1
                 write(zhdr,'(" test",i1,": ")') itest

                 call ezspline_derivative(h220,i1,i2,i3-1,6,xbrak,ybrak,zbrak,&
                      zvec,ier)
                 if(ier.ne.0) call bad_exit
                 zdum=(zvec(6)-zvec(5))/delz
                 write(6,1001) zhdr,zdum,trim(zlbl(itest))
                 zsum=(zorig+zdum)/2
                 zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
                 if(zdiff.gt.zsumtol4) then
                    write(6,*) &
                         ' ------> relative difference: ',zdiff, &
                         ' ******************'
                 endif
              endif
           endif

           !----------------
           ! 3d r8 ezspline

           itest=0
           write(6,*) ' '
           write(6,*) ' 3d r8 ezspline:'

           !  test continuity across grid cell bdy:
           !  continuity not expected for 3rd derivatives.

           cresult = ' '
           ccflag=.true.

           call ezspline_derivative(s3,i1,i2,i3,9,xctest,yconst,zconst, &
                fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(xctest,fctest,cresult,cflag)
           if(i1.lt.3) ccflag = ccflag.and.cflag

           call ezspline_derivative(s3,i1,i2,i3,9,xconst,yctest,zconst, &
                fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(yctest,fctest,cresult,cflag)
           if(i2.lt.3) ccflag = ccflag.and.cflag

           call ezspline_derivative(s3,i1,i2,i3,9,xconst,yconst,zctest, &
                fctest,ier)
           if(ier.ne.0) call bad_exit
           call cont_eval(zctest,fctest,cresult,cflag)
           if(i3.lt.3) ccflag = ccflag.and.cflag

           if(.not.ccflag) then
              write(6,*) '...continuity: ',trim(cresult),' ???????????????????'
           else
              write(6,*) '...continuity: ',trim(cresult)
           endif

           call ezspline_derivative(s3,i1,i2,i3,xtarg,ytarg,ztarg,zval,ier)
           if(ier.ne.0) call bad_exit
           write(6,1001) ' value: ',zval,trim(zlbl0)
           zorig=zval

           if(i1.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(s3,i1-1,i2,i3,6,xbrak,ybrak,zbrak, &
                   zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(2)-zvec(1))/delx
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i2.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(s3,i1,i2-1,i3,6,xbrak,ybrak,zbrak, &
                   zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(4)-zvec(3))/dely
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i3.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(s3,i1,i2,i3-1,6,xbrak,ybrak,zbrak, &
                   zvec,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(6)-zvec(5))/delz
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           !----------------
           ! 3d non-compact spline test
     
           zvalp = zval
     
           itest=0
           write(6,*) ' '
           write(6,*) ' 3d r8 non-compact spline:'

           call ezmake_ict3(i1,i2,i3,ict(1:10))
           call r8tcspeval(xtarg,ytarg,ztarg,ict,zval,x,nx,y,ny,z,nz, &
                ilinx,iliny,ilinz,fffspl,nx,ny,ier)
           if(ier.ne.0) call bad_exit
           write(6,1001) ' value: ',zval,trim(zlbl0)
           zorig=zval

           if(abs(zval-zvalp)/max(1.0_r8,abs(zval)).gt.1.0d-10) then
              write(6,*) ' +++++++++> variation: ezspline r8, non-compact: ', &
                   zvalp,zval
           endif
           
           if(i1.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezmake_ict3(i1-1,i2,i3,ict(1:10))
              call r8tcspvec(ict,6,xbrak,ybrak,zbrak,6,zvec, &
                   nx,xpkg,ny,ypkg,nz,zpkg,fffspl,nx,ny,iwarn,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(2)-zvec(1))/delx
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i2.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest
              
              call ezmake_ict3(i1,i2-1,i3,ict(1:10))
              call r8tcspvec(ict,6,xbrak,ybrak,zbrak,6,zvec, &
                   nx,xpkg,ny,ypkg,nz,zpkg,fffspl,nx,ny,iwarn,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(4)-zvec(3))/dely
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i3.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest
              
              call ezmake_ict3(i1,i2,i3-1,ict(1:10))
              call r8tcspvec(ict,6,xbrak,ybrak,zbrak,6,zvec, &
                   nx,xpkg,ny,ypkg,nz,zpkg,fffspl,nx,ny,iwarn,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvec(6)-zvec(5))/delz
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           !----------------
           ! 3d r4 ezspline

           itest=0
           write(6,*) ' '
           write(6,*) ' 3d r4 ezspline:'

           call ezspline_derivative(t3,i1,i2,i3,xtargr4,ytargr4,ztargr4, &
                zvalr4,ier)
           if(ier.ne.0) call bad_exit
           write(6,1001) ' value: ',zvalr4*1.0_R8,trim(zlbl0)
           zorig=zvalr4

           if(i1.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(t3,i1-1,i2,i3,6,xbrakr4,ybrakr4, &
                   zbrakr4,zvecr4,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvecr4(2)-zvecr4(1))/delx
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i2.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(t3,i1,i2-1,i3,6,xbrakr4,ybrakr4, &
                   zbrakr4,zvecr4,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvecr4(4)-zvecr4(3))/dely
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

           if(i3.gt.0) then
              itest=itest+1
              write(zhdr,'(" test",i1,": ")') itest

              call ezspline_derivative(t3,i1,i2,i3-1,6,xbrakr4,ybrakr4, &
                   zbrakr4,zvecr4,ier)
              if(ier.ne.0) call bad_exit
              zdum=(zvecr4(6)-zvecr4(5))/delz
              write(6,1001) zhdr,zdum,trim(zlbl(itest))
              zsum=(zorig+zdum)/2
              zdiff=abs(zdum-zorig)/max(epslon,abs(zsum))
              if(zdiff.gt.zsumtol4) then
                 write(6,*) &
                   ' ------> relative difference: ',zdiff,' ******************'
              endif
           endif

        enddo
     enddo
  enddo

1001 format(1x,a,1x,1pe13.6,1x,a)

  contains
    subroutine makex(grid,ngrid)

      integer, intent(in) :: ngrid  ! ** should be odd number **
      real*8, intent(out) :: grid(ngrid)

      !  make unevenly spaced grid...

      integer :: inm1,igrid,ii,ictr,ilocal

      !-----------------------------

      grid(1)=0.0_R8
      grid(ngrid)=c2pi

      inm1=ngrid-1  ! even number
      ilocal=3*(inm1/2)

      ictr=0
      igrid=1

      do ii=1,ilocal-1
         ictr=ictr+1
         if(ictr.eq.3) then
            ictr=0
            cycle
         else
            igrid=igrid+1
            if(igrid.ge.ngrid) cycle
            grid(igrid)=ii*c2pi/ilocal
         endif
      enddo

    end subroutine makex

    subroutine maklbl(prefix,i1,i2,i3,result)
      character*(*), intent(in) :: prefix  ! first part of label
      integer,intent(in) :: i1,i2,i3 ! differentiations per coordinate (0:3)
      character*(*), intent(out) :: result

      !------------------------
      ! local:
      integer :: isum,ilen
      character*3 cdx,cdy,cdz
      !------------------------

      isum=i1+i2+i3

      cdx=' '
      if(i1.eq.1) cdx='dx'
      if(i1.gt.1) write(cdx,'("dx",i1)') i1
      cdy=' '
      if(i2.eq.1) cdy='dy'
      if(i2.gt.1) write(cdy,'("dy",i1)') i2
      cdz=' '
      if(i3.eq.1) cdz='dz'
      if(i3.gt.1) write(cdz,'("dz",i1)') i3

      result=' '
      if(isum.eq.0) then
         result = prefix//'f'
      else if(isum.eq.1) then
         result = prefix//'df/'
      else
         write(result,'(a,"d",i1,"f/")') prefix,isum
      endif

      ilen=len(trim(result))

      if(cdx.ne.' ') then
         result(ilen+1:)=cdx
         ilen=len(trim(result))
      endif

      if(cdy.ne.' ') then
         result(ilen+1:)=cdy
         ilen=len(trim(result))
      endif

      if(cdz.ne.' ') then
         result(ilen+1:)=cdz
         ilen=len(trim(result))
      endif

    end subroutine maklbl

    subroutine cont_eval(x,f,clbl,flag)

      !  continuity tester:
      !    cubic extrapolation of f(1:4) to f(5) and 
      !     back extrapolation of f(6:9) to f(5) should match for derivatives
      !     of max order 2 or less; if match occurs append "Yes" else "No" to
      !     the result string "clbl"; also set "flag" accordingly...

      real(r8), intent(in) :: x(9),f(9)
      character*(*), intent(inout) :: clbl
      logical, intent(out) :: flag

      !--------
      !  local:

      integer :: insert
      real*8 :: ans1,ans2,ans3,zdenom,zdiff
      !--------
      !  clear clbl to end; prepare to insert result string

      if(clbl.eq.' ') then
         insert=1
      else
         insert=2+len(trim(clbl))  ! leave a blank space
      endif
      clbl(insert:len(clbl))=' '

      ans3 = f(5)

      !--------
      !  compute 1st cubic extrapolation

      call extrap(f(1),x(2)-x(1),f(2),x(3)-x(1),f(3),x(4)-x(1),f(4), &
           x(5)-x(1),ans1)

      !--------
      !  compute 2nd cubic extrapolation

      call extrap(f(9),x(9)-x(8),f(8),x(9)-x(7),f(7),x(9)-x(6),f(6), &
           x(9)-x(5),ans2)

      !--------
      zdenom=max(1.0_r8,abs(ans1),abs(ans2),abs(ans3))

      zdiff=max(abs(ans1-ans2),abs(ans1-ans3),abs(ans2-ans3))

      if((zdiff/zdenom).gt.1.0e-10_R8) then
         clbl(insert:insert+1)='No'
         flag=.false.
      else
         clbl(insert:insert+2)='Yes'
         flag=.true.
      endif

    end subroutine cont_eval

    subroutine extrap(f0,x1,f1,x2,f2,x3,f3,xans,ans)
      ! cubic extrapolation
      !   (0,f0),(x1,f1),(x2,f2),(x3,f3) --> (xans,ans)

      real(r8), intent(in) :: f0,x1,f1,x2,f2,x3,f3,xans
      real(r8), intent(out) :: ans

      real(r8) :: d1,d2,d3,a,b,c,r1,r2,s

      d1=f1-f0
      d2=f2-f0
      d3=f3-f0

      !  f(x) = a*x**3 + b*x**2 + c*x + f0  --  3 unknowns to solve for...

      r1=x2/x1; r1=r1*r1*r1
      r2=x3/x1; r2=r2*r2*r2

      s=(x3*x3-r2*x1*x1)/(x2*x2-r1*x1*x1)

      c=(d3-r2*d1-s*d2+s*r1*d1)/(x3-r2*x1-s*x2+s*r1*x1)
      b=(d2-r1*d1-c*(x2-r1*x1))/(x2*x2-r1*x1*x1)
      a=(d1-c*x1-b*x1*x1)/(x1*x1*x1)

      !! write(6,*) ' f(x1)=',f0+x1*(c+x1*(b+x1*a)),' = ',f1
      !! write(6,*) ' f(x2)=',f0+x2*(c+x2*(b+x2*a)),' = ',f2
      !! write(6,*) ' f(x3)=',f0+x3*(c+x3*(b+x3*a)),' = ',f3

      !  done.
      !  compute answer

      ans = f0 + xans*(c + xans*(b + xans*a))

    end subroutine extrap

end program qk_pspline
