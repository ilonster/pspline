!/////
! R4 !
!/////
program drive
  !
  ! test drive for EZspline routines
  !
  ! A. Pletzer Mon Apr 24 08:33:20 EDT 2000
  !
  implicit none
  integer ier

  write(0,*)'***********************************************'
  write(0,*)'* EZspline test drive using for _R4 precision *'
  write(0,*)'***********************************************'


  write(0,*)'This program performs 1-d, 2-d and 3-d data interpolation'
  write(0,*)'and derivative evaluations based on both spline and Akima'
  write(0,*)'Hermite representations. The Akima Hermite interpolation is'
  write(0,*)'a Hermite interpolation for which the derivatives of the'
  write(0,*)'function at the nodes are internally evaluated. Boundary '
  write(0,*)'conditions such as "not-a-knot", periodic, 1st and 2nd '
  write(0,*)'derivative imposed are tested, and so are interpolations'
  write(0,*)'performed on a set of isolated points, on a cloud of points'
  write(0,*)'and on a grid array of points (grid).'
  write(0,*)' '
  write(0,*)'If you have MATLAB installed on your system, you can view'
  write(0,*)'the interpolation results saved in the netCDF files *.nc'
  write(0,*)'provided you also have access to MEXCDF, a netCDF to MATLAB'
  write(0,*)'interface package freely available at'
  write(0,*)'http://crusty.er.usgs.gov/~cdenham/MexCDF/nc4ml5.html'
  write(0,*)'To run the MATLAB script ezspline_test.m, simply type'
  write(0,*)'"ezspline_test" at the matlab prompt.'

  write(0,*)''
  write(0,*)'> 1-D splines'
  write(0,*)''
  write(0,*)'>> not-a-knot boundary conditions'
  call spline1_not(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline1_not'
  write(0,*)'>> periodic boundary conditions'
  call spline1_per(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline1_per'
  write(0,*)'>> 1st derivative boundary conditions'
  call spline1_1st(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline1_1st'
  write(0,*)'>> 2nd derivative boundary conditions'
  call spline1_2nd(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline1_2nd'

  
  write(0,*)''
  write(0,*)'> 2-D splines'
  write(0,*)''
  write(0,*)'>> not-a-knot boundary conditions'
  call spline2_not(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline2_not'
  write(0,*)'>> periodic boundary conditions'
  call spline2_per(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline2_per'
  write(0,*)'>> mixed boundary conditions'
  call spline2_mix(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline2_mix'

  write(0,*)''
  write(0,*)'> 3-D splines'
  write(0,*)''
  write(0,*)'>> mixed boundary conditions'
  call spline3_mix(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline3_mix'
  write(0,*)'>> mixed boundary conditions 2'
  call spline3_mox(ier)
  if(ier/=0) write(0,*)'**ERROR** after spline3_mox'

  write(0,*)''
  write(0,*)'> 1-D Akima Hermite'
  write(0,*)''
  call akima1_not(ier)
  if(ier/=0) write(0,*)'**ERROR** after akima1_not'
  write(0,*)'>> periodic boundary conditions'
  call akima1_per(ier)
  if(ier/=0) write(0,*)'**ERROR** after akima1_per'

  write(0,*)''
  write(0,*)'> 2-D Akima Hermite'
  write(0,*)''
  call akima2_not(ier)
  if(ier/=0) write(0,*)'**ERROR** after akima2_not'
  write(0,*)'>> periodic boundary conditions'
  call akima2_per(ier)
  if(ier/=0) write(0,*)'**ERROR** after akima2_per'
  
  write(0,*)''
  write(0,*)'> 3-D Akima Hermite'
  write(0,*)''
  write(0,*)'>> mixed boundary conditions'
  call akima3_mix(ier)
  if(ier/=0) write(0,*)'**ERROR** after akima3_mix'

  stop ' *successful end of EZspline test drive*'
end program drive

! 
! 1-D SPLINE
!

!..........not a knot.....................................................
subroutine spline1_not(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       & f_cloud(k1), f_cloudx(k1)
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ 0, 0 /)
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1

  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_isInDomain(spl,  x1_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' cloud interpolation =>"spline1_not.nc" & "spline1_notx.nc" (exact)'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  f_cloudx = sin(x1_cloud)
  call EZspline_isInDomain(spl, k1, x1_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  
  call EZspline_2netCDF(k1, x1_cloud, f_cloud, 'spline1_not_.nc', ier)
  call EZspline_2netCDF(k1, x1_cloud, f_cloudx,'spline1_notx.nc', ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline1_not

!..........periodic........................................................
subroutine spline1_per(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       & f_cloud(k1), f_cloudx(k1)
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ -1, -1 /) 
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' cloud interpolation => "spline1_per.nc" & "spline1_perx.nc" (exact)'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  f_cloudx = sin(x1_cloud)
  call EZspline_2netCDF( k1, x1_cloud, f_cloud, 'spline1_per_.nc', ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline1_per

!..........1st derivative.....................................................
subroutine spline1_1st(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       & f_cloud(k1), f_cloudx(k1)
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ 1, 1/)
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1
  spl%bcval1min = 1.0_r4
  spl%bcval1max = 1.0_r4

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' cloud interpolation => "spline1_1st.nc" & "spline_1stx.nc" (exact)'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  f_cloudx = sin(x1_cloud)
  call EZspline_2netCDF(k1, x1_cloud, f_cloud, 'spline1_1st_.nc', ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline1_1st

!..........2nd derivative.....................................................
subroutine spline1_2nd(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       & f_cloud(k1), f_cloudx(k1) &
           &   , fx_point, fx_pointx, fxx_point, fxx_pointx
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  real(r4) :: df
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ 2, 2/)
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1
  spl%bcval1min = 0.0_r4
  spl%bcval1max = 0.0_r4

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' cloud interpolation => "spline1_2nd.nc" & "spline1_2nd.nc" (exact)'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  f_cloudx = sin(x1_cloud)
  call Ezspline_2netCDF( k1, x1_cloud, f_cloud, 'spline1_2nd_.nc', ier)

  write(0,*)' point derivative (x, fx, fx-exact, error)'
  x1_point  = twopi/2.23987563_r4
  fx_pointx = cos(x1_point)
  call EZspline_derivative(spl, 1, x1_point, fx_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, fx_point, fx_pointx, &
       fx_point - fx_pointx

  write(0,*)' point derivative (x, fxx, fxx-exact, error)'
  fxx_pointx = -sin(x1_point)
  call EZspline_derivative(spl, 2, x1_point, fxx_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, fxx_point, fxx_pointx, &
       fxx_point - fxx_pointx

  write(0,*)' gradient  (x, fx)'
  call EZspline_gradient(spl, x1_point, df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(2f10.6)') x1_point, df
  write(0,'(2f10.6)') x1_point, fx_pointx

  write(0,*)'save 1-D spline object in file "spline1.nc"'
  call EZspline_save(spl,"spline1.nc", ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline1_2nd

!! 
!! 2-D SPLINE
!!

!..........not a knot.....................................................
subroutine spline2_not(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11
  integer, parameter :: k1 = 21, k2 = 21, k_cloud = 41
  real(r4) :: x1(n1), x2(n2), f(n1, n2)
  real(r4) :: x1_point, x2_point, f_point, f_pointx, &
       & fx_point, fx_pointx, fy_point, fy_pointx, &
       & fxx_point, fxx_pointx, fxy_point, fxy_pointx,  fyy_point, fyy_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), &
       f_array(k1, k2), f_arrayx(k1, k2)
  integer :: bcs1(2), bcs2(2)
  type(EZspline2_r4) spl
  integer i, j
  real(r4) :: x_pt, y_pt
  real(r4) :: df(2)
  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r4)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ 0, 0/) ! not a knot
  bcs2 = (/ 0, 0/) ! not a knot
  write(0,'(" sizes ", i3,"*", i3, "=", i9)') n1, n2, n1*n2
  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1  
  spl%x2 = x2
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x_pt = (1.0_r4 + cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + cos( x1_point ))*sin( x2_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 
  call EZspline_isInDomain(spl,  x1_point, x2_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, x1_point, x2_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "spline2_not_c.nc" & "spline2_not_cx.nc" (exact)'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + cos( x1_cloud ))*sin( x2_cloud ) )**2
  call EZspline_isInDomain(spl,   k_cloud, x1_cloud, x2_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF( k_cloud, x1_cloud, x2_cloud, f_cloud, "spline2_not__c.nc", ier)
  call EZspline_2netCDF( k_cloud, x1_cloud, x2_cloud, f_cloudx,"spline2_notx_c.nc", ier)

  write(0,*)' array interpolation  => "spline2_not_a.nc" & "spline2_not_ax.nc" (exact)'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j) = &
                & ( (1.0_r4 + &
                & cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & cos( x1_array(i) ))*sin( x2_array(j) ) )**2 
        enddo
     enddo
  call EZspline_isInDomain(spl,  k1, k2, x1_array, x2_array, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k1, k2, x1_array, x2_array, f_array, ier)
  call EZspline_error(ier)
  call EZspline_2netCDF( k1, k2, x1_array, x2_array, f_array, "spline2_not__a.nc", ier)
  call EZspline_2netCDF( k1, k2, x1_array, x2_array, f_arrayx,"spline2_notx_a.nc", ier)
  
  write(0,*)' point derivative (x, y, fx, fx-exact, error)'
  fx_pointx = -2_r4*(1_r4 + Cos(x1_point) - Cos(x2_point))*Sin(x1_point)
  call EZspline_derivative(spl, 1, 0, x1_point, x2_point, fx_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fx_point, fx_pointx, &
       fx_point - fx_pointx

  write(0,*)' point derivative (x, y, fy, fy-exact, error)'
  fy_pointx = 2_r4*(1_r4 + Cos(x1_point))*Sin(x2_point)
  call EZspline_derivative(spl, 0, 1, x1_point, x2_point, fy_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fy_point, fy_pointx, &
       fy_point - fy_pointx

  write(0,*)' point derivative (x, y, fxx, fxx-exact, error)'
  fxx_pointx = -2_r4*(Cos(x1_point) + Cos(2_r4*x1_point) - Cos(x1_point)*Cos(x2_point))
  call EZspline_derivative(spl, 2, 0, x1_point, x2_point, fxx_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fxx_point, fxx_pointx, &
       fxx_point - fxx_pointx

  write(0,*)' point derivative (x, y, fyy, fyy-exact, error)'
  fyy_pointx = 2.0_r4*(1.0_r4 + Cos(x1_point))*Cos(x2_point)
  call EZspline_derivative(spl, 0, 2, x1_point, x2_point, fyy_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fyy_point, fyy_pointx, &
       fyy_point - fyy_pointx

  write(0,*)' point derivative (x, y, fxy, fxy-exact, error)'
  fxy_pointx = -2.0_r4*Sin(x1_point)*Sin(x2_point)
  call EZspline_derivative(spl, 1, 1, x1_point, x2_point, fxy_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fxy_point, fxy_pointx, &
       fxy_point - fxy_pointx

  write(0,'(a,2f10.6)')' gradient (x, y, fx, fy)'
  call EZspline_gradient(spl, x1_point, x2_point, &
       & df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return  
  write(0,'(4f10.6)') &
       & x1_point, x2_point, df(1), df(2)
  write(0,'(4f10.6)') &
       & x1_point, x2_point, fx_pointx, fy_pointx

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline2_not
  

!..........periodic.........................................................
subroutine spline2_per(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11
  integer, parameter :: k1 = 21, k2 = 21, k_cloud = 41
  real(r4) :: x1(n1), x2(n2), f(n1, n2)
  real(r4) :: x1_point, x2_point, f_point, f_pointx, &
       & fx_point, fx_pointx, fy_point, fy_pointx, &
       & fxx_point, fxx_pointx, fxy_point, fxy_pointx,  fyy_point, fyy_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), &
       f_array(k1, k2), f_arrayx(k1, k2)
  integer :: bcs1(2), bcs2(2)
  type(EZspline2_r4) spl
  integer i, j
  real(r4) :: x_pt, y_pt

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r4)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ -1, -1/) ! periodic
  bcs2 = (/ -1, -1/) ! periodic
  write(0,'(" sizes ", i3,"*", i3, "=", i9)') n1, n2, n1*n2
  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1  
  spl%x2 = x2
  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x_pt = (1.0_r4 + cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + cos( x1_point ))*sin( x2_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 
  call EZspline_interp(spl, x1_point, x2_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "spline2_per_c.nc" & "spline2_per_cx.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + cos( x1_cloud ))*sin( x2_cloud ) )**2
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, f_cloud, "spline2_per__c.nc", ier)

  write(0,*)' array interpolation "spline2_per_a.nc" & "spline2_per_ax.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j) = &
                & ( (1.0_r4 + &
                & cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & cos( x1_array(i) ))*sin( x2_array(j) ) )**2 
        enddo
     enddo
  call EZspline_interp(spl, k1, k2, x1_array, x2_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, x1_array, x2_array, f_array, "spline2_per__a.nc", ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline2_per


!..........mixed BCs............................................................
subroutine spline2_mix(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11
  integer, parameter :: k1 = 21, k2 = 21, k_cloud = 41
  real(r4) :: x1(n1), x2(n2), f(n1, n2)
  real(r4) :: x1_point, x2_point, f_point, f_pointx, &
       & fx_point, fx_pointx, fy_point, fy_pointx, &
       & fxx_point, fxx_pointx, fxy_point, fxy_pointx,  fyy_point, fyy_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), &
       f_array(k1, k2), f_arrayx(k1, k2)
  integer :: bcs1(2), bcs2(2)
  type(EZspline2_r4) spl
  integer i, j
  real(r4) :: x_pt, y_pt

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r4)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ 1, 2/) ! fx on left, fxx on right
  bcs2 = (/2, 1/)
  write(0,'(" sizes ", i3,"*", i3, "=", i9)') n1, n2, n1*n2
  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1  
  spl%x2 = x2
  spl%bcval1min = -2_r4*(1_r4 + Cos(spl%x1(1)) - Cos(spl%x2))*Sin(spl%x1(1)) 
  spl%bcval1max = -2_r4*( &
       & Cos(spl%x1(n1)) + Cos(2_r4*spl%x1(n1)) - Cos(spl%x1(n1))*Cos(spl%x2) &
       & ) 
  spl%bcval2max = 2_r4*(1_r4 + Cos(spl%x1))*Sin(spl%x2(n2))  ! (..,1)
  spl%bcval2min = 2_r4*(1_r4 + Cos(spl%x1))*Cos(spl%x2(1))   ! (2,..)

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x_pt = (1.0_r4 + cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + cos( x1_point ))*sin( x2_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 
  call EZspline_interp(spl, x1_point, x2_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "spline2_mix_c.nc" & "spline2_mix_cx.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)

  x2_cloud = twopi/2.2345456_r4

  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)

  f_cloudx = ( (1.0_r4 + cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + cos( x1_cloud ))*sin( x2_cloud ) )**2
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, f_cloud, "spline2_mix__c.nc", ier)

  write(0,*)' array interpolation "spline2_mix_a.nc" & "spline2_mix_ax.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j) = &
                & ( (1.0_r4 + &
                & cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & cos( x1_array(i) ))*sin( x2_array(j) ) )**2 
        enddo
     enddo
  call EZspline_interp(spl, k1, k2, x1_array, x2_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, x1_array, x2_array, f_array, "spline2_mix__a.nc", ier)

  write(0,*)'save spline object in file "spline2.nc"'
  call EZspline_save(spl, "spline2.nc", ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline2_mix

!!! 
!!! 3-D SPLINE
!!!

!..........mixed BCs 1..........................................................
subroutine spline3_mix(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11, n3=11
  integer, parameter :: k1=11, k2=11, k3=11, k_cloud = 21
  real(r4) :: x1(n1), x2(n2), x3(n3), f(n1, n2, n3)
  real(r4) :: x1_point, x2_point, x3_point, f_point, f_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), x3_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), x3_array(k3), &
       f_array(k1, k2, k3), f_arrayx(k1, k2, k3)
  integer :: bcs1(2), bcs2(2), bcs3(2) 
  type(EZspline3_r4) spl
  integer i, j, k
  real(r4) :: x_pt, y_pt, z_pt

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r4)/real(n3-1, r4), k = 1, n3) /)

  do k = 1, n3
     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*sin( x2(j) )
           z_pt = x3(k)*sin( x1(i) )
           f(i,j,k) = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
        enddo
     enddo
  enddo

  bcs1 = (/ -1, -1 /) ! periodic
  bcs2 = (/ -1, -1 /) ! periodic
  bcs3 = (/  0,  0 /) ! not a knot
  write(0,'(" sizes ", i3,"*", i3, "*", i3, "=", i9)') n1, n2, n3, n1*n2*n3
  call EZspline_init(spl, n1, n2, n3, bcs1, bcs2, bcs3, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 ! not really necessay here since x1, x2, x3 coincide 
  spl%x2 = x2 ! with default mesh
  spl%x3 = x3
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, z, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x3_point = spl%x3min + (spl%x3max-spl%x3min)/4.93642_r4
  x_pt = (1.0_r4 + x3_point*cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + x3_point*cos( x1_point ))*sin( x2_point )
  z_pt = x3_point*sin( x1_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
  call EZspline_isInDomain(spl, x1_point, x2_point, x3_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  
  call EZspline_interp(spl, x1_point, x2_point, x3_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(5f10.6," ERROR=>",e10.2)') x1_point, x2_point, x3_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "spline3_mix_c.nc" & "spline3_mix_cx.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x3_cloud = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*sin( x2_cloud ) )**2 &
       & + ( x3_cloud*sin( x1_cloud ) )**2
  call EZspline_isInDomain(spl, k_cloud, x1_cloud, x2_cloud, x3_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, &
       & "spline3_mix__c.nc", ier)
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloudx, &
       & "spline3_mix_cx.nc", ier)
  
  write(0,*)' array interpolation => "spline3_mix_a.nc" & "spline3_mix_ax.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
  x3_array = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k3-1,r4), i=1, k3) /)
  do k = 1, k3
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j,k) = &
                & ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*sin( x2_array(j) ) )**2 &
                & + ( x3_array(k)*sin( x1_array(i) ) )**2
        enddo
     enddo
  enddo
  call EZspline_isInDomain(spl, k1, k2, k3, x1_array, x2_array, x3_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  call EZspline_interp(spl, k1, k2, k3, x1_array, x2_array, x3_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, k3, x1_array, x2_array, x3_array, f_array,&
       & "spline3_mix__a.nc", ier)
  call EZspline_2netCDF(k1, k2, k3, x1_array, x2_array, x3_array, f_arrayx,&
       & "spline3_mix_ax.nc", ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline3_mix

!..........mixed BCs 2..........................................................
subroutine spline3_mox(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11, n3=11
  integer, parameter ::k1=11, k2=11, k3=11, k_cloud = 21
  real(r4) :: x1(n1), x2(n2), x3(n3), f(n1, n2, n3)
  real(r4) :: x1_point, x2_point, x3_point, f_point, f_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), x3_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), x3_array(k3), &
       f_array(k1, k2, k3), f_arrayx(k1, k2, k3)
  integer :: bcs1(2), bcs2(2), bcs3(2) 
  type(EZspline3_r4) spl
  integer i, j, k
  real(r4) :: x_pt, y_pt, z_pt
  real(r4), dimension(:,:), allocatable :: a2, a3, b1, b3, c1, c2
  real(r4) df(3)

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r4)/real(n3-1, r4), k = 1, n3) /)

  do k = 1, n3
     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*sin( x2(j) )
           z_pt = x3(k)*sin( x1(i) )
           f(i,j,k) = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
        enddo
     enddo
  enddo

  bcs1 = (/1, 2/) ! fx, fxx
  bcs2 = (/2, 2/) ! fyy, fyy
  bcs3 = (/1, 1/) ! fz, fz
  write(0,'(" sizes ", i3,"*", i3, "*", i3, "=", i9)') n1, n2, n3, n1*n2*n3
  call EZspline_init(spl, n1, n2, n3, bcs1, bcs2, bcs3, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 
  spl%x2 = x2
  spl%x3 = x3

  ! apply boundary conditions

  allocate(a2(spl%n2,spl%n3), a3(spl%n2,spl%n3))
  allocate(b1(spl%n1,spl%n3), b3(spl%n1,spl%n3))
  allocate(c1(spl%n1,spl%n2), c2(spl%n1,spl%n2))

  a2 = spread(spl%x2, dim=2, ncopies=spl%n3)
  a3 = spread(spl%x3, dim=1, ncopies=spl%n2)
  spl%bcval1min = -4_r4*a3*Sin(spl%x1min)*Sin(a2/2._r4)**2
  spl%bcval1max = -4_r4*a3*Cos(spl%x1max)*Sin(a2/2._r4)**2

  b1 = spread(spl%x1, dim=2, ncopies=spl%n3)
  b3 = spread(spl%x3, dim=1, ncopies=spl%n1)
  spl%bcval2min = 2_r4*(1_r4 + b3*Cos(b1))*Cos(spl%x2min)
  spl%bcval2max = 2_r4*(1_r4 + b3*Cos(b1))*Cos(spl%x2max)

  c1 = spread(spl%x1, dim=2, ncopies=spl%n2)
  c2 = spread(spl%x2, dim=1, ncopies=spl%n1)
  spl%bcval3min = 2_r4*spl%x3min + 2_r4*Cos(c1) - Cos(c1 - c2) - Cos(c1 + c2)
  spl%bcval3max = 2_r4*spl%x3max + 2_r4*Cos(c1) - Cos(c1 - c2) - Cos(c1 + c2)
  
  deallocate(a2, a3)
  deallocate(b1, b3)
  deallocate(c1, c2)
  
  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, z, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x3_point = spl%x3min + (spl%x3max-spl%x3min)/4.93642_r4
  x_pt = (1.0_r4 + x3_point*cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + x3_point*cos( x1_point ))*sin( x2_point )
  z_pt = x3_point*sin( x1_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
  call EZspline_interp(spl, x1_point, x2_point, x3_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(5f10.6," ERROR=>",e10.2)') x1_point, x2_point, x3_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,'(a,3f10.6)')' gradient (x, y, z, fx, fy, fz)'
  call EZspline_gradient(spl, x1_point, x2_point, x3_point, &
       & df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(10f10.6)') x1_point, x2_point, x3_point, &
       & df(1), df(2), df(3)
  !compute exact values
  df(1)= -4_r4*x3_point*Sin(x1_point)*Sin(x2_point/2_r4)**2
  df(2)= 2_r4*(1_r4 + x3_point*Cos(x1_point))*Sin(x2_point)
  df(3)= 2_r4*x3_point + 2_r4*Cos(x1_point) - &
       & Cos(x1_point - x2_point) - Cos(x1_point + x2_point)
   write(0,'(10f10.6)') x1_point, x2_point, x3_point, &
       & df(1), df(2), df(3)
  
  write(0,*)' cloud interpolation => "spline3_mox__c.nc" & "spline3_mox_cx.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x3_cloud = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*sin( x2_cloud ) )**2 &
       & + ( x3_cloud*sin( x1_cloud ) )**2
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, &
       & "spline3_mox__c.nc", ier)

  write(0,*)' array interpolation => "spline3_mox__a.nc" & "spline3_mox_ax.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
  x3_array = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k3-1,r4), i=1, k3) /)
  do k = 1, k3
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j,k) = &
                & ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*sin( x2_array(j) ) )**2 &
                & + ( x3_array(k)*sin( x1_array(i) ) )**2
        enddo
     enddo
  enddo
  call EZspline_interp(spl, k1, k2, k3, x1_array, x2_array, x3_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, k3, x1_array, x2_array, x3_array, f_array, &
       & 'spline3_mox__a.nc',ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine spline3_mox



! 
! 1-D AKIMA
!

!..........standard BC.....................................................
subroutine akima1_not(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       f_cloud(k1), f_cloudx(k1)
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  real(r4) df
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ 0, 0 /)
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1

  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  ! For Akima Hermite represention
  spl%isHermite = 1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_isInDomain(spl,  x1_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' cloud interpolation =>"akima1__not.nc"'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  f_cloudx = sin(x1_cloud)
  call EZspline_isInDomain(spl, k1, x1_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  

  write(0,*)' gradient  (x, fx)'
  call EZspline_gradient(spl, x1_point, df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(2f10.6)') x1_point, df
  write(0,'(2f10.6)') x1_point, cos(x1_point)

  call EZspline_2netCDF(k1, x1_cloud, f_cloud, "akima1__not.nc", ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine akima1_not

!..........periodic........................................................
subroutine akima1_per(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1 = 11, k1 = 21
  real(r4) :: x1(n1), f(n1), x1_point, f_point, x1_cloud(k1), &
       & f_cloud(k1), f_cloudx(k1)
  type(EZspline1_r4) :: spl
  integer i, bcs1(2)
  real(r4) df
  
  ier = 0
  x1 = twopi*(/ (real(i-1,r4)/real(n1-1,r4), i=1, n1) /)
  f = sin(x1)

  bcs1 = (/ -1, -1 /) 
  write(0,*)'grid size ',n1
  call EZspline_init(spl, n1, bcs1, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  spl%x1 = x1

  ! For Akima Hermite represention
  spl%isHermite = 1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  write(0,*)' point interpolation (x, f, f-exact, error)'
  x1_point = spl%x1(1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = twopi/4.0_r4
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)

  x1_point = spl%x1(spl%n1)
  call EZspline_interp(spl, x1_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return
  write(0,'(3f10.6," ERROR=>",e10.2)') x1_point, f_point, &
       & sin(x1_point), f_point-sin(x1_point)
  
  write(0,*)' gradient  (x, fx)'
  call EZspline_gradient(spl, x1_point, df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(2f10.6)') x1_point, df
  write(0,'(2f10.6)') x1_point, cos(x1_point)

  write(0,*)' cloud interpolation => "akima1__per.nc"'
  x1_cloud = twopi*(/ (real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  call EZspline_interp(spl, k1, x1_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  f_cloudx = sin(x1_cloud)
  call EZspline_2netCDF( k1, x1_cloud, f_cloud, "akima1__per.nc", ier)

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine akima1_per


!! 
!! 2-D AKIMA HERMITE
!!

!..........not a knot.....................................................
subroutine akima2_not(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11
  integer, parameter :: k1 = 21, k2 = 21, k_cloud = 41
  real(r4) :: x1(n1), x2(n2), f(n1, n2)
  real(r4) :: x1_point, x2_point, f_point, f_pointx, &
       & fx_point, fx_pointx, fy_point, fy_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), &
       f_array(k1, k2), f_arrayx(k1, k2)
  integer :: bcs1(2), bcs2(2)
  type(EZspline2_r4) spl
  integer i, j
  real(r4) :: x_pt, y_pt
  real(r4) df(2)

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r4)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ 0, 0/) ! not a knot
  bcs2 = (/ 0, 0/) ! not a knot
  write(0,'(" sizes ", i3,"*", i3, "=", i9)') n1, n2, n1*n2
  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1  
  spl%x2 = x2
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  ! For Akima Hermite represention
  spl%isHermite = 1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x_pt = (1.0_r4 + cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + cos( x1_point ))*sin( x2_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 
  call EZspline_isInDomain(spl,  x1_point, x2_point, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, x1_point, x2_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "akima2_not__c.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + cos( x1_cloud ))*sin( x2_cloud ) )**2
  call EZspline_isInDomain(spl,   k_cloud, x1_cloud, x2_cloud, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF( k_cloud, x1_cloud, x2_cloud, f_cloud, "akima2_not__c.nc", ier)

  write(0,*)' array interpolation  => "akima2_not__a.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j) = &
                & ( (1.0_r4 + &
                & cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & cos( x1_array(i) ))*sin( x2_array(j) ) )**2 
        enddo
     enddo
  call EZspline_isInDomain(spl,  k1, k2, x1_array, x2_array, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  call EZspline_interp(spl, k1, k2, x1_array, x2_array, f_array, ier)
  call EZspline_error(ier)
  call EZspline_2netCDF( k1, k2, x1_array, x2_array, f_array, "akima2_not__a.nc", ier)
  
  write(0,*)' point derivative (x, y, fx, fx-exact, error)'
  fx_pointx = -2_r4*(1_r4 + Cos(x1_point) - Cos(x2_point))*Sin(x1_point)
  call EZspline_derivative(spl, 1, 0, x1_point, x2_point, fx_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fx_point, fx_pointx, &
       fx_point - fx_pointx

  write(0,*)' point derivative (x, y, fy, fy-exact, error)'
  fy_pointx = 2_r4*(1_r4 + Cos(x1_point))*Sin(x2_point)
  call EZspline_derivative(spl, 0, 1, x1_point, x2_point, fy_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, fy_point, fy_pointx, &
       fy_point - fy_pointx

  write(0,'(a,2f10.6)')' gradient (x, y, fx, fy)'
  call EZspline_gradient(spl, x1_point, x2_point, &
       & df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return  
  write(0,'(4f10.6)') &
       & x1_point, x2_point, df(1), df(2)
  write(0,'(4f10.6)') &
       & x1_point, x2_point, fx_pointx, fy_pointx


  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine akima2_not
  

!..........periodic.........................................................
subroutine akima2_per(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11
  integer, parameter :: k1 = 21, k2 = 21, k_cloud = 41
  real(r4) :: x1(n1), x2(n2), f(n1, n2)
  real(r4) :: x1_point, x2_point, f_point, f_pointx, &
       & fx_point, fx_pointx, fy_point, fy_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), &
       f_array(k1, k2), f_arrayx(k1, k2)
  integer :: bcs1(2), bcs2(2)
  type(EZspline2_r4) spl
  integer i, j
  real(r4) :: x_pt, y_pt
  real(r4) df(2)

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)

     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + cos( x1(i) ))*sin( x2(j) )
           f(i,j) = (x_pt-1.0_r4)**2 + y_pt**2
        enddo
     enddo

  bcs1 = (/ -1, -1/) ! periodic
  bcs2 = (/ -1, -1/) ! periodic
  write(0,'(" sizes ", i3,"*", i3, "=", i9)') n1, n2, n1*n2
  call EZspline_init(spl, n1, n2, bcs1, bcs2, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1  
  spl%x2 = x2

  ! For Akima Hermite represention
  spl%isHermite = 1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x_pt = (1.0_r4 + cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + cos( x1_point ))*sin( x2_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 
  call EZspline_interp(spl, x1_point, x2_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(4f10.6," ERROR=>",e10.2)') x1_point, x2_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "akima2_per__c.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + cos( x1_cloud ))*sin( x2_cloud ) )**2
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, f_cloud, 'akima2_per__c.nc', ier)

  write(0,*)' array interpolation "akima2_per__a.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j) = &
                & ( (1.0_r4 + &
                & cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & cos( x1_array(i) ))*sin( x2_array(j) ) )**2 
        enddo
     enddo
  call EZspline_interp(spl, k1, k2, x1_array, x2_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, x1_array, x2_array, f_array, 'akima2_per__a.nc', ier)

  write(0,'(a,2f10.6)')' gradient (x, y, fx, fy)'
  call EZspline_gradient(spl, x1_point, x2_point, &
       & df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return  
  write(0,'(4f10.6)') &
       & x1_point, x2_point, df(1), df(2)
  fx_pointx = -2_r4*(1_r4 + Cos(x1_point) - Cos(x2_point))*Sin(x1_point)
  fy_pointx = 2_r4*(1_r4 + Cos(x1_point))*Sin(x2_point)
  write(0,'(4f10.6)') &
       & x1_point, x2_point, fx_pointx, fy_pointx

  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine akima2_per



!!! 
!!! 3-D AKIMA HERMITE
!!!

!..........mixed BCs..........................................................
subroutine akima3_mix(ier)
  use EZspline_obj
  use EZspline
  implicit none
  integer, intent(out) :: ier

  integer, parameter :: r4 = selected_real_kind(6,37)
  real(r4), parameter :: twopi = 6.2831853071795865
  integer, parameter :: n1=11, n2=11, n3=11
  integer, parameter :: k1=11, k2=11, k3=11, k_cloud = 21
  real(r4) :: x1(n1), x2(n2), x3(n3), f(n1, n2, n3)
  real(r4) :: x1_point, x2_point, x3_point, f_point, f_pointx
  real(r4) :: x1_cloud(k_cloud), x2_cloud(k_cloud), x3_cloud(k_cloud), &
       & f_cloud(k_cloud), f_cloudx(k_cloud)
  real(r4) :: x1_array(k1), x2_array(k2), x3_array(k3), &
       f_array(k1, k2, k3), f_arrayx(k1, k2, k3)
  integer :: bcs1(2), bcs2(2), bcs3(2) 
  type(EZspline3_r4) spl
  integer i, j, k
  real(r4) :: x_pt, y_pt, z_pt
  real(r4) df(3)

  ier = 0

  x1 = twopi* (/ ( real(i-1,r4)/real(n1-1, r4), i = 1, n1) /)
  x2 = twopi* (/ ( real(j-1,r4)/real(n2-1, r4), j = 1, n2) /)
  x3 =        (/ ( real(k-1,r4)/real(n3-1, r4), k = 1, n3) /)

  do k = 1, n3
     do j = 1, n2
        do i = 1, n1
           x_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*cos( x2(j) )
           y_pt = (1.0_r4 + x3(k)*cos( x1(i) ))*sin( x2(j) )
           z_pt = x3(k)*sin( x1(i) )
           f(i,j,k) = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
        enddo
     enddo
  enddo

  bcs1 = (/ -1, -1 /) ! periodic
  bcs2 = (/ -1, -1 /) ! periodic
  bcs3 = (/  0,  0 /) ! not a knot
  write(0,'(" sizes ", i3,"*", i3, "*", i3, "=", i9)') n1, n2, n3, n1*n2*n3
  call EZspline_init(spl, n1, n2, n3, bcs1, bcs2, bcs3, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  spl%x1 = x1 ! not really necessay here since x1, x2, x3 coincide 
  spl%x2 = x2 ! with default mesh
  spl%x3 = x3
  call EZspline_isGridRegular(spl, ier)
  call EZspline_error(ier)
  if(ier /=0 ) return

  ! For Akima Hermite represention
  spl%isHermite = 1

  call EZspline_setup(spl, f, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
 
  write(0,*)' point interpolation (x, y, z, f, f-exact, error)'
  x1_point = spl%x1min + (spl%x1max-spl%x1min)/2.34656_r4
  x2_point = spl%x2min + (spl%x2max-spl%x2min)/1.36482_r4
  x3_point = spl%x3min + (spl%x3max-spl%x3min)/4.93642_r4
  x_pt = (1.0_r4 + x3_point*cos( x1_point ))*cos( x2_point )
  y_pt = (1.0_r4 + x3_point*cos( x1_point ))*sin( x2_point )
  z_pt = x3_point*sin( x1_point )
  f_pointx = (x_pt-1.0_r4)**2 + y_pt**2 + z_pt**2
  call EZspline_isInDomain(spl, x1_point, x2_point, x3_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  
  call EZspline_interp(spl, x1_point, x2_point, x3_point, f_point, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(5f10.6," ERROR=>",e10.2)') x1_point, x2_point, x3_point, &
       & f_point, f_pointx, f_point-f_pointx

  write(0,*)' cloud interpolation => "akima3_mix__c.nc"'
  x1_cloud = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x2_cloud = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  x3_cloud = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k_cloud-1,r4), i=1, k_cloud) /)
  f_cloudx = ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*cos( x2_cloud) - 1.0_r4 )**2 &
       & + ( (1.0_r4 + x3_cloud*cos( x1_cloud ))*sin( x2_cloud ) )**2 &
       & + ( x3_cloud*sin( x1_cloud ) )**2
  call EZspline_isInDomain(spl, k_cloud, x1_cloud, x2_cloud, x3_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  
  call EZspline_interp(spl, k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k_cloud, x1_cloud, x2_cloud, x3_cloud, f_cloud, &
       & "akima3_mix__c.nc", ier)
  
  write(0,*)' array interpolation => "akima3_mix__a.nc"'
  x1_array = spl%x1min + (spl%x1max-spl%x1min)* &
       & (/(real(i-1,r4)/real(k1-1,r4), i=1, k1) /)
  x2_array = spl%x2min + (spl%x2max-spl%x2min)* &
       & (/(real(i-1,r4)/real(k2-1,r4), i=1, k2) /)
  x3_array = spl%x3min + (spl%x3max-spl%x3min)* &
       & (/(real(i-1,r4)/real(k3-1,r4), i=1, k3) /)
  do k = 1, k3
     do j = 1, k2
        do i = 1, k1
           f_arrayx(i,j,k) = &
                & ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*cos( x2_array(j)) - 1.0_r4 )**2 &
                & + ( (1.0_r4 + &
                & x3_array(k)*cos( x1_array(i) ))*sin( x2_array(j) ) )**2 &
                & + ( x3_array(k)*sin( x1_array(i) ) )**2
        enddo
     enddo
  enddo
  call EZspline_isInDomain(spl, k1, k2, k3, x1_array, x2_array, x3_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return

  call EZspline_interp(spl, k1, k2, k3, x1_array, x2_array, x3_array, f_array, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  call EZspline_2netCDF(k1, k2, k3, x1_array, x2_array, x3_array, f_array,&
       & "akima3_mix__a.nc", ier)

  write(0,'(a,3f10.6)')' gradient (x, y, z, fx, fy, fz)'
  call EZspline_gradient(spl, x1_point, x2_point, x3_point, &
       & df, ier)
  call EZspline_error(ier)
  if(ier /= 0) return
  write(0,'(10f10.6)') x1_point, x2_point, x3_point, &
       & df(1), df(2), df(3)
  !compute exact values
  df(1)= -4_r4*x3_point*Sin(x1_point)*Sin(x2_point/2_r4)**2
  df(2)= 2_r4*(1_r4 + x3_point*Cos(x1_point))*Sin(x2_point)
  df(3)= 2_r4*x3_point + 2_r4*Cos(x1_point) - &
       & Cos(x1_point - x2_point) - Cos(x1_point + x2_point)
   write(0,'(10f10.6)') x1_point, x2_point, x3_point, &
       & df(1), df(2), df(3)
  
  call EZspline_free(spl, ier)
  call EZspline_error(ier)

end subroutine akima3_mix

