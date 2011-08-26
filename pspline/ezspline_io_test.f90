program ezspline_io_test

  !  typical compilation:
  !    <fortran-driver> -c -I<module-path> ezspline_io_test.f90
  !  typical load:
  !    <fortran-driver> ezspline_io_test.o \
  !       $libpath/pspline.a  $libpath/ezcdf.a  $libpath/portlib.a \
  !       -L$netCDF_path -lnetcdf
  !
  !  demonstrate ezspline_save and ezspline_load with new options
  !  dmc March 2006.

  use ezSpline_obj
  use ezSpline
  implicit NONE

  type(EZspline3_r8) :: fs, gs

  integer, parameter :: nx=21, nth=31, nph=41
  integer :: ix,ith,iph,ier

  real*8 :: x(nx),th(nth),ph(nph), f(nx,nth,nph), g(nx,nth,nph)
  integer :: bc(2) = (/ 0.d0, 0.d0 /)
  integer :: bcperio(2) = (/ -1.0d0, -1.0d0 /)

  real*8, parameter :: C2PI = 6.2831853071795862D+00
  real*8, parameter :: ONE = 1.0d0

  real*8 :: xtest(5) = (/ 0.0d0, 0.2d0, 0.4d0, 0.6d0, 0.8d0 /)
  real*8 :: thtest(5) = (/ 0.5d0, 0.6d0, 0.7d0, 0.8d0, 0.9d0 /)
  real*8 :: phtest(5) = (/ 2.5d0, 2.6d0, 2.7d0, 2.8d0, 2.9d0 /)

  real*8 :: ftest(5), gtest(5)

  !-----------------------------------

  do ix=1,nx
     x(ix)=(ix-1)*ONE/(nx-1)
  enddo

  do ith=1,nth
     th(ith)=(ith-1)*C2PI/(nth-1)
  enddo

  do iph=1,nph
     ph(iph)=(iph-1)*C2PI/(nph-1)
  enddo

  do iph=1,nph
     do ith=1,nth
        do ix=1,nx

           f(ix,ith,iph) = (1-x(ix)**2)*cos(th(ith))*sin(ph(iph))
           g(ix,ith,iph) = (1-x(ix)**2)**2*sin(th(ith))*cos(ph(iph))

        enddo
     enddo
  enddo

  !  nullify pointers; put fs & gs into a known, empty state:
  call EZspline_preInit(fs)
  call EZspline_preInit(gs)

  !  create 2 3d splines

  write(6,*) ' initialize 3d spline objects...'

  call EZspline_init(fs, nx,nth,nph, bc,bcperio,bcperio, ier); call ckerr
  call EZspline_init(gs, nx,nth,nph, bc,bcperio,bcperio, ier); call ckerr

  fs%x1 = x
  fs%x2 = th
  fs%x3 = ph

  gs%x1 = x
  gs%x2 = th
  gs%x3 = ph

  write(6,*) ' load data and calculate spline coefficients...'

  call ezspline_setup(fs, f, ier); call ckerr
  call ezspline_setup(gs, g, ier); call ckerr

  !  evaluate a few points

  call fgeval

  write(6,*) ' save splines with all coefficients to "foo.cdf"...'

  call jsystem('rm -f foo.cdf') ! handy "portlib" call...

  call ezspline_save(fs, "foo.cdf", ier, "f", fullsave=.TRUE.); call ckerr
  call ezspline_save(gs, "foo.cdf", ier, "g", fullsave=.TRUE.); call ckerr

  write(6,*) ' free the splines... '

  call ezspline_free(fs,ier); call ckerr
  call ezspline_free(gs,ier); call ckerr

  write(6,*) ' reload splines from file "foo.cdf"...'

  call ezspline_load(fs, "foo.cdf", ier, "f")
  call ezspline_load(gs, "foo.cdf", ier, "g")

  !  evaluate a few points

  call fgeval

  write(6,*) ' test completed.'

  contains

    subroutine fgeval

      write(6,*) ' '
      write(6,*) ' test evaluations... '

      call ezspline_interp(fs, size(xtest), xtest, thtest, phtest, ftest, ier)
      call ckerr

      call ezspline_interp(gs, size(xtest), xtest, thtest, phtest, gtest, ier)
      call ckerr

      !  write them out

      do ix=1,size(xtest)
         write(6,'(1x,i2,". ",5(1x,1pe13.6))') ix, &
              xtest(ix),thtest(ix),phtest(ix),ftest(ix),gtest(ix)
      enddo

      write(6,*) ' '

    end subroutine fgeval

    subroutine ckerr

      if(ier.ne.0) then
         write(6,*) ' ?foo: unexpected error:'
         call ezspline_error(ier)
         call bad_exit
      endif

    end subroutine ckerr

end program ezspline_io_test
