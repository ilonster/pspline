!/////
! R8 !
!/////
program drive2
  !
  ! performance test
  !
  use EZspline_obj
  use EZspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8) t(12)
  type(ezspline1_r8) :: spl1
  type(ezspline2_r8) :: spl2
  type(ezspline3_r8) :: spl3
  integer n, ier, i

  write(*,*)' '
  write(*,*)' 1-D'
  write(*,*)' '

  n = 4096
  call setup1d(spl1, n, t(1))
  write(*,'(a,i7,a,f10.3)')'time for setup [s] (',n,')=>',&
       & t(1)

  n=1000000
  call interp1d_point(spl1, n, t(2))
  write(*,'(a,i7,a,f10.3)')'time for point interp [s] (',n,')=>',&
       & t(2)


  call interp1d_cloud(spl1, n, t(3))
  write(*,'(a,i7,a,f10.3)')'time for cloud interp [s] (',n,')=>',&
       & t(3)


  call interp1d_array(spl1, n, t(4))
  write(*,'(a,i7,a,f10.3)')'time for array interp [s] (',n,')=>',&
       & t(4)

  call ezspline_free(spl1, ier)

  write(*,*)' '
  write(*,*)' 2-D'
  write(*,*)' '

  n = 64
  call setup2d(spl2, n, t(5))
  write(*,'(a,i7,a,f10.3)')'time for setup [s] (',n,'^2)=>',&
       & t(5)

  n=1000
  call interp2d_point(spl2, n, t(6))
  write(*,'(a,i7,a,f10.3)')'time for point interp [s] (',n,'^2)=>',&
       & t(6)


  call interp2d_cloud(spl2, n, t(7))
  write(*,'(a,i7,a,f10.3)')'time for cloud interp [s] (',n,'^2)=>',&
       & t(7)


  call interp2d_array(spl2, n, t(8))
  write(*,'(a,i7,a,f10.3)')'time for array interp [s] (',n,'^2)=>',&
       & t(8)

  call ezspline_free(spl2, ier)

  write(*,*)' '
  write(*,*)' 3-D'
  write(*,*)' '

  n = 16
  call setup3d(spl3, n, t(9))
  write(*,'(a,i7,a,f10.3)')'time for setup [s] (',n,'^3)=>',&
       & t(9)

  n=100
  call interp3d_point(spl3, n, t(10))
  write(*,'(a,i7,a,f10.3)')'time for point interp [s] (',n,'^3)=>',&
       & t(10)


  call interp3d_cloud(spl3, n, t(11))
  write(*,'(a,i7,a,f10.3)')'time for cloud interp [s] (',n,'^3)=>',&
       & t(11)


  call interp3d_array(spl3, n, t(12))
  write(*,'(a,i7,a,f10.3)')'time for array interp [s] (',n,'^3)=>',&
       & t(12)

  call ezspline_free(spl3, ier)

  write(*,*)'All cpu times [s]'
  write(*,'(12f7.2)') t(1:12)

  stop '* successful termination of drive2'
end program drive2


!
! 1-D
!

subroutine setup1d(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline1_r8), intent(inout) :: spl
  integer, intent(in) :: ki

  real(r8) :: x1(ki), f(ki)
  integer :: bcs1(2)
  real(r8) :: x_pt, t
  integer tics_per_second, tic, toc

  integer :: n1, i, ier

  n1 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)

        do i = 1, n1
           x_pt = 1.0_r8 + cos( x1(i) )
           f(i) = (x_pt-1.0_r8)**2
        enddo

  bcs1 = (/ -1, -1 /) ! periodic

  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 ! not really necessay here since x1 coincides 
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call system_clock(tic, tics_per_second)
  call EZspline_setup(spl, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)
  call EZspline_error(ier)
  if(ier /= 0) return

end subroutine setup1d

subroutine interp1d_point(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline1_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8) :: x1(ki), f(ki)
  integer i, ier, n1
  integer tics_per_second, tic, toc

  n1 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)

  call ezspline_isindomain(spl, n1, x1, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
        do i=1, n1
           call ezspline_interp(spl, x1(i), f(i), ier)
        enddo
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

end subroutine interp1d_point


subroutine interp1d_cloud(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline1_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1, f
  integer i, ier, icount, n1, n, iok
  integer tics_per_second, tic, toc

  n1 = ki

  allocate(x1(ki), f(ki), stat=iok)
  if(iok/=0) stop 'allocation error'
  
  icount = 1
        do i=1, n1
           x1(icount) = spl%x1min + &
                & (spl%x1max-spl%x1min)*real(i-1,r8)/real(n1-1,r8)
           icount = icount + 1
        enddo

  n = n1
  call ezspline_isindomain(spl, n, x1, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n, x1, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1, f)

end subroutine interp1d_cloud

subroutine interp1d_array(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline1_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1
  real(r8), dimension(:), allocatable ::  f
  integer i, ier, n1, iok
  integer tics_per_second, tic, toc

  n1 = ki

  allocate(x1(n1), stat=iok)
  if(iok/=0) stop 'allocation error'
  allocate(f(n1), stat=iok)
  if(iok/=0) stop 'allocation error'

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)

  call ezspline_isindomain(spl, n1, x1, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n1, x1, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1,f)

end subroutine interp1d_array

!!
!! 2-D
!!

subroutine setup2d(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline2_r8), intent(inout) :: spl
  integer, intent(in) :: ki

  real(r8) :: x1(ki), x2(ki), f(ki,ki)
  integer :: bcs1(2), bcs2(2)
  real(r8) :: x_pt, y_pt, t
  integer tics_per_second, tic, toc

  integer :: n1, n2, i, j, ier

  n1 = ki
  n2 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r8 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r8 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r8)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ -1, -1 /) ! periodic
  bcs2 = (/ -1, -1 /) ! periodic

  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 ! not really necessay here since x1, x2 coincide 
  spl%x2 = x2 ! with default mesh
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call system_clock(tic, tics_per_second)
  call EZspline_setup(spl, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)
  call EZspline_error(ier)
  if(ier /= 0) return

end subroutine setup2d

subroutine interp2d_point(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline2_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8) :: x1(ki), x2(ki), f(ki,ki)
  integer i, j, ier, n1, n2
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)

  call ezspline_isindomain(spl, n1, n2, x1, x2, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
     do j=1, n2
        do i=1, n1
           call ezspline_interp(spl, x1(i), x2(j), f(i,j), ier)
        enddo
     enddo
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

end subroutine interp2d_point


subroutine interp2d_cloud(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline2_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1, x2, f
  integer i, j, k, ier, icount, n1, n2, n, iok
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki

  allocate(x1(ki**2), x2(ki**2), f(ki**2), stat=iok)
  if(iok/=0) stop 'allocation error'
  
  icount = 1
     do j=1, n2
        do i=1, n1
           x1(icount) = spl%x1min + &
                & (spl%x1max-spl%x1min)*real(i-1,r8)/real(n1-1,r8)
           x2(icount) = spl%x2min + &
                & (spl%x2max-spl%x2min)*real(i-1,r8)/real(n2-1,r8)
           icount = icount + 1
        enddo
     enddo

  n = n1*n2
  call ezspline_isindomain(spl, n, x1, x2, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n, x1, x2, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1, x2, f)

end subroutine interp2d_cloud

subroutine interp2d_array(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline2_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1, x2
  real(r8), dimension(:,:), allocatable ::  f
  integer i, j, ier, n1, n2, iok
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki

  allocate(x1(n1), x2(n2), stat=iok)
  if(iok/=0) stop 'allocation error'
  allocate(f(n1,n2), stat=iok)
  if(iok/=0) stop 'allocation error'

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)

  call ezspline_isindomain(spl, n1, n2, x1, x2, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n1, n2, x1, x2, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1,x2,f)

end subroutine interp2d_array


!!!
!!! 3-D
!!!

subroutine setup3d(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline3_r8), intent(inout) :: spl
  integer, intent(in) :: ki

  real(r8) :: x1(ki), x2(ki), x3(ki), f(ki,ki,ki)
  integer :: bcs1(2), bcs2(2), bcs3(2)
  real(r8) :: x_pt, y_pt, z_pt, t
  integer tics_per_second, tic, toc

  integer :: n1, n2, n3, i, j, k, ier

  n1 = ki
  n2 = ki
  n3 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r8)/real(n3-1, r8), k = 1, n3) /)

  do k = 1, n3
     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r8 + x3(k)*cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r8 + x3(k)*cos( x1(i) ))*sin( x2(j) )
           z_pt = x3(k)*sin( x1(i) )
           f(i,j,k) = (x_pt-1.0_r8)**2 + y_pt**2 + z_pt**2
        enddo
     enddo
  enddo

  bcs1 = (/ -1, -1 /) ! periodic
  bcs2 = (/ -1, -1 /) ! periodic
  bcs3 = (/  0,  0 /) ! not a knot

  call EZspline_init(spl, n1, n2, n3, bcs1, bcs2, bcs3, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 ! not really necessay here since x1, x2, x3 coincide 
  spl%x2 = x2 ! with default mesh
  spl%x3 = x3
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call system_clock(tic, tics_per_second)
  call EZspline_setup(spl, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)
  call EZspline_error(ier)
  if(ier /= 0) return

end subroutine setup3d

subroutine interp3d_point(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline3_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8) :: x1(ki), x2(ki), x3(ki), f(ki,ki,ki)
  integer i, j, k, ier, n1, n2, n3
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki
  n3 = ki

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r8)/real(n3-1, r8), k = 1, n3) /)

  call ezspline_isindomain(spl, n1, n2, n3, x1, x2, x3, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  do k=1, n3
     do j=1, n2
        do i=1, n1
           call ezspline_interp(spl, x1(i), x2(j), x3(k), f(i,j,k), ier)
        enddo
     enddo
  enddo
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

end subroutine interp3d_point


subroutine interp3d_cloud(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline3_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1, x2, x3, f
  integer i, j, k, ier, icount, n1, n2, n3, n, iok
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki
  n3 = ki

  allocate(x1(ki**3), x2(ki**3), x3(ki**3), f(ki**3), stat=iok)
  if(iok/=0) stop 'allocation error'
  
  icount = 1
  do k=1, n3
     do j=1, n2
        do i=1, n1
           x1(icount) = spl%x1min + &
                & (spl%x1max-spl%x1min)*real(i-1,r8)/real(n1-1,r8)
           x2(icount) = spl%x2min + &
                & (spl%x2max-spl%x2min)*real(i-1,r8)/real(n2-1,r8)
           x3(icount) = spl%x3min + &
                & (spl%x3max-spl%x3min)*real(i-1,r8)/real(n3-1,r8)
           icount = icount + 1
        enddo
     enddo
  enddo

  n = n1*n2*n3
  call ezspline_isindomain(spl, n, x1, x2, x3, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n, x1, x2, x3, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1, x2, x3, f)

end subroutine interp3d_cloud

subroutine interp3d_array(spl, ki, t)
  use ezspline_obj
  use ezspline
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  real(r8), parameter :: twopi = 6.2831853071795865

  type(ezspline3_r8), intent(in) :: spl
  integer, intent(in) :: ki
  real(r8), intent(out) :: t

  real(r8), dimension(:), allocatable :: x1, x2, x3
  real(r8), dimension(:,:,:), allocatable ::  f
  integer i, j, k, ier, n1, n2, n3, iok
  integer tics_per_second, tic, toc

  n1 = ki
  n2 = ki
  n3 = ki

  allocate(x1(n1), x2(n2), x3(n3), stat=iok)
  if(iok/=0) stop 'allocation error'
  allocate(f(n1,n2,n3), stat=iok)
  if(iok/=0) stop 'allocation error'

  x1 = twopi* (/ ( real(i-1,r8)/real(n1-1, r8), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r8)/real(n2-1, r8), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r8)/real(n3-1, r8), k = 1, n3) /)

  call ezspline_isindomain(spl, n1, n2, n3, x1, x2, x3, ier)
  if(ier/=0) print*,'out of domain error'

  call system_clock(tic, tics_per_second)
  call ezspline_interp(spl, n1, n2, n3, x1, x2, x3, f, ier)
  call system_clock(toc, tics_per_second)
  t = real(toc-tic)/real(tics_per_second)

  deallocate(x1,x2,x3,f)

end subroutine interp3d_array
