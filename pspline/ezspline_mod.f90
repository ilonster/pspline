module EZspline_type
 
  integer, parameter  :: ezspline_r8 = selected_real_kind(12,100)
  integer, parameter  :: ezspline_r4 = selected_real_kind(6,37)
  real(ezspline_r8), parameter :: ezspline_twopi_r8 = 6.2831853071795865_ezspline_r8
  real(ezspline_r4), parameter :: ezspline_twopi_r4 = 6.2831853071795865_ezspline_r4
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EZspline data types
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  type EZspline3_r8
     !
     ! 3-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r8), dimension(:), pointer :: x1, x2, x3
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r8), dimension(:,:), pointer :: bcval1min, bcval1max
     real(ezspline_r8), dimension(:,:), pointer :: bcval2min, bcval2max
     real(ezspline_r8), dimension(:,:), pointer :: bcval3min, bcval3max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline, Akima or Hybrid; =1 for piecewise linear: this is set
     ! by EZspline_init, EZhybrid_init, or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! set =0 by init routines other than EZhybrid_init which sets it =1:
     integer :: isHybrid
     !
     ! the following is set by EZhybrid_init; other EZ*_init routines clear:
     integer :: hspline(3)  ! interpolation code along each dimension
     !        -1: zonal step fcn; =0: pc linear; =1: Akima Hermite; =2: Spline
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1, n2, n3
     !
     ! Grid zone lookup method
     !
     integer :: klookup1,klookup2,klookup3
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives a set via  bcval1min. (See above.)
     !
     integer ibctype1(2), ibctype2(2), ibctype3(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r8) :: x1min, x1max, x2min, x2max, x3min, x3max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r8), dimension(:,:,:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1, ilin2, ilin3
     real(ezspline_r8), dimension(:,:), pointer :: x1pkg, x2pkg, x3pkg
     !
     integer :: nguard
  end type EZspline3_r8

  type EZspline2_r8
     !
     ! 2-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r8), dimension(:), pointer :: x1, x2
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r8), dimension(:), pointer :: bcval1min, bcval1max
     real(ezspline_r8), dimension(:), pointer :: bcval2min, bcval2max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline, Akima or Hybrid; =1 for piecewise linear: this is set
     ! by EZspline_init, EZhybrid_init, or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! set =0 by init routines other than EZhybrid_init which sets it =1:
     integer :: isHybrid
     !
     ! the following is set by EZhybrid_init; other EZ*_init routines clear:
     integer :: hspline(2)  ! interpolation code along each dimension
     !        -1: zonal step fcn; =0: pc linear; =1: Akima Hermite; =2: Spline
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1, n2
     !
     ! Grid zone lookup method
     !
     integer :: klookup1,klookup2
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives are set via  bcval1min. (See above)
     !
     integer ibctype1(2), ibctype2(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r8) :: x1min, x1max, x2min, x2max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r8), dimension(:,:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1, ilin2
     real(ezspline_r8), dimension(:,:), pointer :: x1pkg, x2pkg
     !
     integer :: nguard
  end type EZspline2_r8
 
  type EZspline1_r8
     !
     ! 1-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r8), dimension(:), pointer :: x1
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r8) :: bcval1min, bcval1max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline or Akima; =1 for piecewise linear: this is set
     ! by EZspline_init or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1
     !
     ! Grid zone lookup method
     !
     integer :: klookup1
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives are set via  bcval1min. (See above)
     !
     integer ibctype1(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r8) :: x1min, x1max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r8), dimension(:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1
     real(ezspline_r8), dimension(:,:), pointer :: x1pkg
     !
     integer :: nguard
  end type EZspline1_r8

  type EZspline3_r4
     !
     ! 3-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r4), dimension(:), pointer :: x1, x2, x3
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r4), dimension(:,:), pointer :: bcval1min, bcval1max
     real(ezspline_r4), dimension(:,:), pointer :: bcval2min, bcval2max
     real(ezspline_r4), dimension(:,:), pointer :: bcval3min, bcval3max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline, Akima or Hybrid; =1 for piecewise linear: this is set
     ! by EZspline_init, EZhybrid_init, or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! set =0 by init routines other than EZhybrid_init which sets it =1:
     integer :: isHybrid
     !
     ! the following is set by EZhybrid_init; other EZ*_init routines clear:
     integer :: hspline(3)  ! interpolation code along each dimension
     !        -1: zonal step fcn; =0: pc linear; =1: Akima Hermite; =2: Spline
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1, n2, n3
     !
     ! Grid zone lookup method
     !
     integer :: klookup1,klookup2,klookup3
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives a set via  bcval1min. (See above.)
     !
     integer ibctype1(2), ibctype2(2), ibctype3(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r4) :: x1min, x1max, x2min, x2max, x3min, x3max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r4), dimension(:,:,:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1, ilin2, ilin3
     real(ezspline_r4), dimension(:,:), pointer :: x1pkg, x2pkg, x3pkg
     !
     integer :: nguard
  end type EZspline3_r4

  type EZspline2_r4
     !
     ! 2-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r4), dimension(:), pointer :: x1, x2
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r4), dimension(:), pointer :: bcval1min, bcval1max
     real(ezspline_r4), dimension(:), pointer :: bcval2min, bcval2max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline, Akima or Hybrid; =1 for piecewise linear: this is set
     ! by EZspline_init, EZhybrid_init, or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! set =0 by init routines other than EZhybrid_init which sets it =1:
     integer :: isHybrid
     !
     ! the following is set by EZhybrid_init; other EZ*_init routines clear:
     integer :: hspline(2)  ! interpolation code along each dimension
     !        -1: zonal step fcn; =0: pc linear; =1: Akima Hermite; =2: Spline
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1, n2
     !
     ! Grid zone lookup method
     !
     integer :: klookup1,klookup2
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives are set via  bcval1min. (See above)
     !
     integer ibctype1(2), ibctype2(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r4) :: x1min, x1max, x2min, x2max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r4), dimension(:,:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1, ilin2
     real(ezspline_r4), dimension(:,:), pointer :: x1pkg, x2pkg
     !
     integer :: nguard
  end type EZspline2_r4
 
  type EZspline1_r4
     !
     ! 1-d Spline/Akima Hermite/Piecewise Linear interpolation
     !
     ! Grid
     !
     real(ezspline_r4), dimension(:), pointer :: x1
     !
     ! The boundary condition values (for slope and 2nd derivative).
     ! Can be optionally set by the user. Not used for periodic and
     ! not a knot boundary conditions.
     !
     real(ezspline_r4) :: bcval1min, bcval1max
     !
     ! Select between spline (0) and Akima spline (1); default=0 (spline)
     !
     integer :: isHermite  ! set after EZspline_init call...
     !
     ! set =0 for Spline or Akima; =1 for piecewise linear: this is set
     ! by EZspline_init or EZlinear_init; DO NOT SET DIRECTLY:
     !
     integer :: isLinear
     !
     ! Grid sizes (set during EZ*_init call).
     !
     integer :: n1
     !
     ! Grid zone lookup method
     !
     integer :: klookup1
     !
     ! Type of boundary conditions (set during EZspline_init call) on left
     ! and right hand side. Possible values are:
     !
     ! -1 periodic
     ! 0 not a knot
     ! +1 1st derivative imposed
     ! +2 2nd derivative imposed
     !
     ! For instance, ibctype1 =(/1, 0/) for 1st derivative set on left-hand
     ! and not a knot boundary conditions on right-hand side. The values of
     ! the derivatives are set via  bcval1min. (See above)
     !
     integer ibctype1(2)
     !
     ! Grid lengths. DO NOT SET.
     !
     real(ezspline_r4) :: x1min, x1max
     !
     ! Compact cubic coefficient arrays. DO NOT SET.
     !
     real(ezspline_r4), dimension(:,:), pointer :: fspl
     !
     ! Control/Other. DO NOT SET.
     !
     integer :: isInitialized, isAllocated, isReady
     integer :: ilin1
     real(ezspline_r4), dimension(:,:), pointer :: x1pkg
     !
     integer :: nguard
  end type EZspline1_r4

!=========================================================================
! End type
end module EZspline_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EZspline reflective methods (first argument is an EZspline1,2,3 type).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module EZspline_obj
  use Ezspline_type
  interface EZspline_preInit
     !
     ! initialize the "isAllocated" and "nguard" elements of EZspline object.
     ! spline_o%nguard = 123456789 means EZspline_preInit or EZspline_init
     !                  or EZlinear_init has been called
     ! spline_o%isAllocated = 1 means object is initialized & allocated
     ! spline_o%isAllocated = 0 means object is de-allocated
     ! note that these values are initially undefined...!
     !
     ! usage:  call EZspline_preinit(spline_o)
     !   ...where spline_o is a 1d, 2d, or 3d spline object of R4 or R8
     !   precision.

     module procedure &
          EZspline_preInit1_r8, &
          EZspline_preInit2_r8, &
          EZspline_preInit3_r8, &
          EZspline_preInit1_r4, &
          EZspline_preInit2_r4, &
          EZspline_preInit3_r4

  end interface

  interface EZspline_allocated
     ! logical function returns TRUE if allocated, FALSE otherwise
     !
     ! usage:  
     !   logical :: answer
     !   answer = EZspline_allocated(spline_o)
     !   ...where spline_o is a 1d, 2d, or 3d spline object of R4 or R8
     !   precision.

     module procedure &
          EZspline_allocated1_r8, &
          EZspline_allocated2_r8, &
          EZspline_allocated3_r8, &
          EZspline_allocated1_r4, &
          EZspline_allocated2_r4, &
          EZspline_allocated3_r4
  end interface

  contains

    subroutine EZspline_preInit1_r8(spline_o)
      use EZspline_type
      type(EZspline1_r8) :: spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit1_r8

    subroutine EZspline_preInit2_r8(spline_o)
      use EZspline_type
      type(EZspline2_r8) :: spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit2_r8

    subroutine EZspline_preInit3_r8(spline_o)
      use EZspline_type
      type(EZspline3_r8) spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit3_r8

    subroutine EZspline_preInit1_r4(spline_o)
      use EZspline_type
      type(EZspline1_r4) spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit1_r4

    subroutine EZspline_preInit2_r4(spline_o)
      use EZspline_type
      type(EZspline2_r4) spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit2_r4

    subroutine EZspline_preInit3_r4(spline_o)
      use EZspline_type
      type(EZspline3_r4) spline_o
      spline_o%nguard=123456789
      spline_o%isAllocated=0
      spline_o%isReady=0
      spline_o%isInitialized=0
    end subroutine EZspline_preInit3_r4


    logical function EZspline_allocated1_r8(spline_o)
      use EZspline_type
      type(EZspline1_r8) spline_o
      EZspline_allocated1_r8 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated1_r8 = .TRUE.
         endif
      endif
    end function EZspline_allocated1_r8

    logical function EZspline_allocated2_r8(spline_o)
      use EZspline_type
      type(EZspline2_r8) spline_o
      EZspline_allocated2_r8 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated2_r8 = .TRUE.
         endif
      endif
    end function EZspline_allocated2_r8

    logical function EZspline_allocated3_r8(spline_o)
      use ezspline_type
      type(EZspline3_r8) spline_o
      EZspline_allocated3_r8 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated3_r8 = .TRUE.
         endif
      endif
    end function EZspline_allocated3_r8

    logical function EZspline_allocated1_r4(spline_o)
      use ezspline_type
      type(EZspline1_r4) spline_o
      EZspline_allocated1_r4 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated1_r4 = .TRUE.
         endif
      endif
    end function EZspline_allocated1_r4

    logical function EZspline_allocated2_r4(spline_o)
      use ezspline_type
      type(EZspline2_r4) spline_o
      EZspline_allocated2_r4 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated2_r4 = .TRUE.
         endif
      endif
    end function EZspline_allocated2_r4

    logical function EZspline_allocated3_r4(spline_o)
      use ezspline_type
      type(EZspline3_r4) spline_o
      EZspline_allocated3_r4 = .FALSE.
      if(spline_o%nguard==123456789) then
         if(spline_o%isAllocated==1) then
            EZspline_allocated3_r4 = .TRUE.
         endif
      endif
    end function EZspline_allocated3_r4

    subroutine ezmake_ict1(i,ict)
      !  (private utility for ezspline derivative2 subroutines)
      !  make ict(1:6) array
      !  for higher derivatives; d[i1+i2]f/dx[i1]dy[i2]
      !  expecting i in range [0:3] (NOT CHECKED)

      implicit NONE
      integer, intent(in) :: i
      integer, intent(out) :: ict(3)

      if(i.eq.0) then
         ict = (/1, 0, 0 /) ! seek f @ (p1)
      else if(i.eq.1) then
         ict = (/0, 1, 0 /) ! df/dx
      else if(i.eq.2) then
         ict = (/0, 0, 1 /) ! d2f/dx2
      else
         ict = (/3, 0, 0 /) ! d3f/dx3
      endif

    end subroutine ezmake_ict1

    subroutine ezmake_ict2(i1,i2,ict)
      !  (private utility for ezspline derivative2 subroutines)
      !  make ict(1:6) array
      !  for higher derivatives; d[i1+i2]f/dx[i1]dy[i2]
      !  expecting i1 & i2 in range [0:3] (NOT CHECKED)

      implicit NONE
      integer, intent(in) :: i1,i2
      integer, intent(out) :: ict(6)

      integer :: imark,isum,iii

      !  this generates the control argument needed by evbicub & similar
      !  routines...
      !----------------------

      isum = i1+i2
      ict(1)=isum

      imark=0

      if(isum.eq.0) then
         ict = (/1, 0, 0, 0, 0, 0 /) ! seek f @ (p1, p2, p3)
      else if(isum.eq.1) then
         if(i1.eq.1) then
            ict = (/0, 1, 0, 0, 0, 0 /) ! df/dx
         else
            ict = (/0, 0, 1, 0, 0, 0 /) ! df/dy
         endif
      else if(isum.eq.2) then
         if(i1.eq.2) then
            ict = (/0, 0, 0, 1, 0, 0 /) ! d2f/dx2
         else if(i2.eq.2) then
            ict = (/0, 0, 0, 0, 1, 0 /) ! d2f/dy2
         else
            ict = (/0, 0, 0, 0, 0, 1 /) ! d2f/dxdy
         endif
      else if(isum.eq.3) then
         if(i1.eq.3) then
            imark=2  ! fxxx
         else if(i1.eq.2) then
            imark=3  ! fxxy
         else if(i1.eq.1) then
            imark=4  ! fxyy
         else
            imark=5  ! fyyy
         endif
      else if(isum.eq.4) then
         if(i1.eq.3) then
            imark=2  ! fxxxy
         else if(i2.eq.3) then
            imark=4  ! fxyyy
         else
            imark=3  ! fxxyy
         endif
      else if(isum.eq.5) then
         if(i1.eq.3) then
            imark=2  ! fxxxyy
         else if(i2.eq.3) then
            imark=3  ! fxxyyy
         endif
      endif

      !  isum=6 --> fxxxyyy

      if(isum.gt.2) then
         do iii=2,6
            if(iii.eq.imark) then
               ict(iii)=1
            else
               ict(iii)=0
            endif
         enddo
      endif

    end subroutine ezmake_ict2

    subroutine ezmake_ict3(i1,i2,i3,ict)
      !  (private utility for ezspline derivative3 subroutines)
      !  make ict(1:10) array
      !  for higher derivatives; d[i1+i2+i3]f/dx[i1]dy[i2]dz[i3]
      !  i1 & i2 & i3 in range [0:3] (NOT CHECKED)

      implicit NONE
      integer, intent(in) :: i1,i2,i3
      integer, intent(out) :: ict(10)

      integer :: imark,isum,iii

      !  this generates the control argument needed by evtricub & similar
      !  routines...
      !----------------------

      isum = i1+i2+i3
      if(max(i1,i2,i3).eq.3) then
         isum=-isum
      endif
      ict(1)=isum

      imark=0

      if(isum.eq.0) then
         ict = (/1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /) ! seek f @ (p1, p2, p3)

      else if(isum.eq.1) then
!  1st derivatives
         if(i1.eq.1) then
            ict = (/0, 1, 0, 0, 0, 0, 0, 0, 0, 0 /)  ! df/dx
         else if(i2.eq.1) then
            ict = (/0, 0, 1, 0, 0, 0, 0, 0, 0, 0 /)  ! df/dy
         else
            ict = (/0, 0, 0, 1, 0, 0, 0, 0, 0, 0 /)  ! df/dz
         endif

      else if(isum.eq.2) then
!  2nd derivatives -- legacy ordering; x-precedence ordering for all
!  higher derivatives...

         if(i1.eq.2) then
            ict = (/0, 0, 0, 0, 1, 0, 0, 0, 0, 0 /)  ! d2f/dx2
         else if(i2.eq.2) then
            ict = (/0, 0, 0, 0, 0, 1, 0, 0, 0, 0 /)  ! d2f/dy2
         else if(i3.eq.2) then
            ict = (/0, 0, 0, 0, 0, 0, 1, 0, 0, 0 /)  ! d2f/dz2
         else if(i3.eq.0) then
            ict = (/0, 0, 0, 0, 0, 0, 0, 1, 0, 0 /)  ! d2f/dxdy
         else if(i2.eq.0) then
            ict = (/0, 0, 0, 0, 0, 0, 0, 0, 1, 0 /)  ! d2f/dxdz
         else
            ict = (/0, 0, 0, 0, 0, 0, 0, 0, 0, 1 /)  ! d2f/dydz
         endif
        
      else if(isum.eq.3) then
!  3rd derivative, continuous: max(i1,i2,i3)<3
         if(i1.eq.2) then
            if(i2.eq.1) then
               imark=2     ! fxxy
            else
               imark=3     ! fxxz
            endif
         else if(i1.eq.1) then
            if(i2.eq.2) then
               imark=4     ! fxyy
            else if(i2.eq.1) then
               imark=5     ! fxyz
            else
               imark=6     ! fxzz
            endif
         else
            if(i2.eq.2) then
               imark=7     ! fyyz
            else
               imark=8     ! fyzz
            endif
         endif

      else if(isum.eq.-3) then
!  3rd derivative
         if(i1.eq.3) then
            imark=2        ! fxxx
         else if(i2.eq.3) then
            imark=3        ! fyyy
         else if(i3.eq.3) then
            imark=4        ! fzzz
         endif

      else if(isum.eq.4) then
!  4th derivative, continuous: max(i1,i2,i3)<3
         if(i1.eq.2) then
            if(i2.eq.2) then
               imark=2     ! fxxyy
            else if(i2.eq.1) then
               imark=3     ! fxxyz
            else
               imark=4     ! fxxzz
            endif
         else if(i1.eq.1) then
            if(i2.eq.2) then
               imark=5     ! fxyyz
            else
               imark=6     ! fxyzz
            endif
         else
            imark=7        ! fyyzz
         endif

      else if(isum.eq.-4) then
!  4th derivative
         if(i1.eq.3) then
            if(i2.eq.1) then
               imark=2     ! fxxxy
            else
               imark=3     ! fxxxz
            endif
         else if(i1.eq.1) then
            if(i2.eq.3) then
               imark=4     ! fxyyy
            else
               imark=5     ! fxzzz
            endif
         else
            if(i2.eq.3) then
               imark=6     ! fyyyz
            else
               imark=7     ! fyzzz
            endif
         endif

      else if(isum.eq.5) then
!  5th derivative, continuous: max(i1,i2,i3)<3
         if(i3.eq.1) then
            imark=2     ! fxxyyz
         else if(i2.eq.1) then
            imark=3     ! fxxyzz
         else
            imark=4     ! fxyyzz
         endif

      else if(isum.eq.-5) then
!  5th derivative
         if(i1.eq.3) then
            if(i2.eq.2) then
               imark=2  ! fxxxyy
            else if(i2.eq.1) then
               imark=3  ! fxxxyz
            else
               imark=4  ! fxxxzz
            endif
         else if(i1.eq.2) then
            if(i2.eq.3) then
               imark=5  ! fxxyyy
            else
               imark=6  ! fxxzzz
            endif
         else if(i1.eq.1) then
            if(i2.eq.3) then
               imark=7  ! fxyyyz
            else
               imark=8  ! fxyzzz
            endif
         else
            if(i2.eq.3) then
               imark=9  ! fyyyzz
            else
               imark=10 ! fyyzzz
            endif
         endif

!  isum=6 --> fxxyyzz  (i1=i2=i3=2)
      else if(isum.eq.-6) then
!  6th derivative
         if(i1.eq.3) then
            if(i2.eq.3) then
               imark=2  ! fxxxyyy
            else if(i2.eq.2) then
               imark=3  ! fxxxyyz
            else if(i2.eq.1) then
               imark=4  ! fxxxyzz
            else
               imark=5  ! fxxxzzz
            endif
         else if(i1.eq.2) then
            if(i2.eq.3) then
               imark=6  ! fxxyyyz
            else if(i2.eq.1) then
               imark=7  ! fxxyzzz
            endif
         else if(i1.eq.1) then
            if(i2.eq.3) then
               imark=8  ! fxyyyzz
            else
               imark=9  ! fxyyzzz
            endif
         else
            imark=10    ! fyyyzzz
         endif

!  isum=7 not possible
      else if(isum.eq.-7) then
!  7th derivative
         if(i1.eq.3) then
            if(i2.eq.3) then
               imark=2  ! fxxxyyyz
            else if(i2.eq.2) then
               imark=3  ! fxxxyyzz
            else
               imark=4  ! fxxxyzzz
            endif
         else if(i1.eq.2) then
            if(i2.eq.3) then
               imark=5  ! fxxyyyzz
            else
               imark=6  ! fxxyyzzz
            endif
         else
            imark=7     ! fxyyyzzz
         endif

!  isum=8 not possible
      else if(isum.eq.-8) then
!  8th derivative
         if(i3.eq.2) then 
            imark=2  ! fxxxyyyzz
         else if(i2.eq.2) then
            imark=3  ! fxxxyyzzz
         else
            imark=4  ! fxxyyyzzz
         endif

!  isum=9 not possible
!  isum=-9 --> fxxxyyyzzz

      endif

      if(abs(isum).gt.2) then
         do iii=2,10
            if(iii.eq.imark) then
               ict(iii)=1
            else
               ict(iii)=0
            endif
         enddo
      endif

    end subroutine ezmake_ict3

end module EZspline_obj

!=======================================================================
 
module EZspline

  interface EZspline_init
     !
     ! Initialize and allocate memory. BCS1,2,3 determine the type of boundary
     ! conditions on either end of the x1,2,3 grids: eg (/0, 0/) for not-a-knot
     ! on the left and (/-1, -1/) periodic. Other BCs such as imposed slope or
     ! second derivative can also be applied by setting '1' or '2' respectively
     ! on either side. For instance (/1, 2/) for 1st derivative on the left and
     ! 2nd derivative on the right. The value of the 1st/2nd derivative must be
     ! set explicitely set through the bcval1,2,3min and bcval1,2,3max arrays.
     !
     subroutine EZspline_init3_r8(spline_o, n1, n2, n3, BCS1, BCS2, BCS3, ier)
       use EZspline_obj
       type(EZspline3_r8) :: spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(in) :: BCS1(2), BCS2(2), BCS3(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init3_r8

     subroutine EZspline_init2_r8(spline_o, n1, n2, BCS1, BCS2, ier)
       use EZspline_obj
       type(EZspline2_r8) :: spline_o
       integer, intent(in) :: n1, n2
       integer, intent(in) :: BCS1(2), BCS2(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init2_r8
 
     subroutine EZspline_init1_r8(spline_o, n1, BCS1, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(in) :: n1
       integer, intent(in) :: BCS1(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init1_r8
 
     subroutine EZspline_init3_r4(spline_o, n1, n2, n3, BCS1, BCS2, BCS3, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(in) :: BCS1(2), BCS2(2), BCS3(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init3_r4
 
     subroutine EZspline_init2_r4(spline_o, n1, n2, BCS1, BCS2, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: n1, n2
       integer, intent(in) :: BCS1(2), BCS2(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init2_r4
 
     subroutine EZspline_init1_r4(spline_o, n1, BCS1, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(in) :: n1
       integer, intent(in) :: BCS1(2)
       integer, intent(out) :: ier
     end subroutine EZspline_init1_r4

  end interface


  interface EZlinear_init
     !
     ! Initialize and allocate memory for piecewise LINEAR interpolation
     ! object.  This is C0.  For C2 cubic spline or C1 Akima Hermite spline,
     ! please see EZspline_init.
     !
     ! No boundary conditions are needed for piecewise linear interpolation
     !
     subroutine EZlinear_init3_r8(spline_o, n1, n2, n3, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(out) :: ier
     end subroutine EZlinear_init3_r8
 
     subroutine EZlinear_init2_r8(spline_o, n1, n2, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: n1, n2
       integer, intent(out) :: ier
     end subroutine EZlinear_init2_r8
 
     subroutine EZlinear_init1_r8(spline_o, n1, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(in) :: n1
       integer, intent(out) :: ier
     end subroutine EZlinear_init1_r8
 
     subroutine EZlinear_init3_r4(spline_o, n1, n2, n3, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(out) :: ier
     end subroutine EZlinear_init3_r4
 
     subroutine EZlinear_init2_r4(spline_o, n1, n2, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: n1, n2
       integer, intent(out) :: ier
     end subroutine EZlinear_init2_r4
 
     subroutine EZlinear_init1_r4(spline_o, n1, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(in) :: n1
       integer, intent(out) :: ier
     end subroutine EZlinear_init1_r4
 
  end interface


  interface EZhybrid_init
     !
     ! Initialize and allocate memory for hybrid interpolation object:
     ! dimensionality > 1 only.  Interpolation method is specified separately
     ! for each dimension.  At present, Akima Hermite and Spline interpolation
     ! cannot be mixed.
     !
     ! Boundary condition arguments are optional.  They are appropriate only
     ! for the end points of dimensions for which Hermite or Spline cubic
     ! interpolation is used.
     !
     ! hspline(...) specifies the interpolation method for each dimension,
     ! according to the code: -1 for step function, 0 for piecewise linear,
     ! 1 for Akima Hermite, 2 for cubic Spline.
     !
     subroutine EZhybrid_init3_r8(spline_o, n1, n2, n3, hspline, ier, &
          BCS1, BCS2, BCS3)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(in) :: hspline(3)
       integer, intent(out) :: ier
       integer, intent(in), OPTIONAL :: BCS1(2), BCS2(2), BCS3(2)
     end subroutine EZhybrid_init3_r8
 
     subroutine EZhybrid_init2_r8(spline_o, n1, n2, hspline, ier, &
          BCS1, BCS2)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: n1, n2
       integer, intent(in) :: hspline(2)
       integer, intent(out) :: ier
       integer, intent(in), OPTIONAL :: BCS1(2), BCS2(2)
     end subroutine EZhybrid_init2_r8

     subroutine EZhybrid_init3_r4(spline_o, n1, n2, n3, hspline, ier, &
          BCS1, BCS2, BCS3)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: n1, n2, n3
       integer, intent(in) :: hspline(3)
       integer, intent(out) :: ier
       integer, intent(in), OPTIONAL :: BCS1(2), BCS2(2), BCS3(2)
     end subroutine EZhybrid_init3_r4
 
     subroutine EZhybrid_init2_r4(spline_o, n1, n2, hspline, ier, &
          BCS1, BCS2)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: n1, n2
       integer, intent(in) :: hspline(2)
       integer, intent(out) :: ier
       integer, intent(in), OPTIONAL :: BCS1(2), BCS2(2)
     end subroutine EZhybrid_init2_r4

  end interface
 
  interface EZspline_free
     !
     ! Reset and free the memory. This method must be called to avoid
     ! memory leaks after all interpolations have been computed.
     !
     subroutine EZspline_free3_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free3_r8
 
     subroutine EZspline_free2_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free2_r8
 
     subroutine EZspline_free1_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free1_r8
 
     subroutine EZspline_free3_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free3_r4
 
     subroutine EZspline_free2_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free2_r4
 
     subroutine EZspline_free1_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_free1_r4
  end interface
 
  interface EZspline_setup
     !
     ! Compute the cubic spline coefficients. Note: the grid and the
     ! boundary conditions should be properly set prior to this call.
     !
     ! NEW optional argument: exact_dim=TRUE to requre f dimensions to
     ! match higher dimensions of spline_o%fspl exactly; default or FALSE
     ! means f dimensions can match or exceed dimensions of spline_o%fspl.
     !
     ! array arguments are now declared with f90 style dimensioning; the
     ! module interface must be used (if not feasible see ezspline_setupx.f90).
     ! 
     subroutine EZspline_setup3_r8(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       real(ezspline_r8), dimension(:,:,:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup3_r8
 
     subroutine EZspline_setup2_r8(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       real(ezspline_r8), dimension(:,:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup2_r8
 
     subroutine EZspline_setup1_r8(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       real(ezspline_r8), dimension(:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup1_r8
 
     subroutine EZspline_setup3_r4(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       real(ezspline_r4), dimension(:,:,:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup3_r4
 
     subroutine EZspline_setup2_r4(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       real(ezspline_r4), dimension(:,:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup2_r4
 
     subroutine EZspline_setup1_r4(spline_o, f, ier, exact_dim)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       real(ezspline_r4), dimension(:), intent(in) :: f
       integer, intent(out) :: ier
       logical, intent(in), optional :: exact_dim
     end subroutine EZspline_setup1_r4
 
  end interface
 
 
 
  interface EZspline_interp
     !
     ! Interpolation at grid point(s) p1, [p2, [p3]]. Result is returned in
     ! f. Interpolation can be sought at a single point (p1, [p2, [p3]] are
     ! scalars), on an unordered list of points (p1, [p2, [p3]] have dimension
     ! k), or on a structured grid (p1, [p2, [p3]] have dimension k1, [k2, [k3]]
     ! respectively).
     !
     subroutine EZspline_interp3_r8(spline_o, p1, p2, p3, f, ier)
       ! single point evaluation
       use EZspline_obj
       type(EZspline3_r8) spline_o
       real(ezspline_r8) :: p1, p2, p3
       real(ezspline_r8) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_r8
 
     subroutine EZspline_interp3_array_r8(spline_o, k1, k2, k3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer :: k1, k2, k3
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r8), intent(out):: f(k1,k2,*)
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_array_r8
 
     subroutine EZspline_interp3_cloud_r8(spline_o, k, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r8), intent(out):: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_cloud_r8
 
     subroutine EZspline_interp2_r8(spline_o, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       real(ezspline_r8) :: p1, p2
       real(ezspline_r8) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_r8
 
     subroutine EZspline_interp2_array_r8(spline_o, k1, k2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer :: k1, k2
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r8), intent(out):: f(k1,*)
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_array_r8
 
     subroutine EZspline_interp2_cloud_r8(spline_o, k, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k)
       real(ezspline_r8), intent(out):: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_cloud_r8
 
     subroutine EZspline_interp1_r8(spline_o, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       real(ezspline_r8) :: p1
       real(ezspline_r8) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp1_r8
 
     subroutine EZspline_interp1_array_r8(spline_o, k1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer :: k1
       real(ezspline_r8), intent(in) :: p1(k1)
       real(ezspline_r8), intent(out):: f(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_interp1_array_r8
 
     subroutine EZspline_interp3_r4(spline_o, p1, p2, p3, f, ier)
       ! single point evaluation
       use EZspline_obj
       type(EZspline3_r4) spline_o
       real(ezspline_r4) :: p1, p2, p3
       real(ezspline_r4) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_r4
 
     subroutine EZspline_interp3_array_r4(spline_o, k1, k2, k3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer :: k1, k2, k3
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r4), intent(out):: f(k1,k2,*)
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_array_r4
 
     subroutine EZspline_interp3_cloud_r4(spline_o, k, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r4), intent(out):: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_interp3_cloud_r4
 
     subroutine EZspline_interp2_r4(spline_o, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       real(ezspline_r4) :: p1, p2
       real(ezspline_r4) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_r4
 
     subroutine EZspline_interp2_array_r4(spline_o, k1, k2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer :: k1, k2
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r4), intent(out):: f(k1,*)
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_array_r4
 
     subroutine EZspline_interp2_cloud_r4(spline_o, k, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k)
       real(ezspline_r4), intent(out):: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_interp2_cloud_r4
 
     subroutine EZspline_interp1_r4(spline_o, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       real(ezspline_r4) :: p1
       real(ezspline_r4) f
       integer, intent(out) :: ier
     end subroutine EZspline_interp1_r4
 
     subroutine EZspline_interp1_array_r4(spline_o, k1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer :: k1
       real(ezspline_r4), intent(in) :: p1(k1)
       real(ezspline_r4), intent(out):: f(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_interp1_array_r4
 
  end interface
 
  interface EZspline_derivative
     !
     ! Evaluate the spline/Akima Hermite derivative of order
     ! d^{i1} d^{i2} d^{i3} f / d x1^{i1} d x2^{i2} d x2^{i2}
     ! at p1, [p2, [p3]]. The sum of i1+[i2+[i3]] should be <=2 for spline, or
     ! <=1 for Akima Hermite or Piecewise Linear. 
     ! Result is return in f. The evaluation can
     ! be sought at a single point (p1, [p2, [p3]] are scalars), on
     ! an unordered list of points (p1, [p2, [p3]] have dimension k), or
     ! on a structured grid (p1, [p2, [p3]] have dimension k1, [k2, [k3]]
     ! respectively).
     !
     !
     subroutine EZspline_derivative3_r8(spline_o, i1, i2, i3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: i1, i2, i3
       real(ezspline_r8), intent(in) :: p1, p2, p3
       real(ezspline_r8), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_r8
 
     subroutine EZspline_derivative3_array_r8(spline_o, i1, i2, i3, &
          & k1, k2, k3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: i1, i2, i3, k1, k2, k3
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r8), intent(out) :: f(k1, k2, *)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_array_r8
 
     subroutine EZspline_derivative3_cloud_r8(spline_o, i1, i2, i3, &
          & k, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: i1, i2, i3, k
       real(ezspline_r8), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r8), intent(out) :: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_cloud_r8
 
     subroutine EZspline_derivative2_r8(spline_o, i1, i2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: i1, i2
       real(ezspline_r8), intent(in) :: p1, p2
       real(ezspline_r8), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_r8
 
     subroutine EZspline_derivative2_array_r8(spline_o, i1, i2, k1, k2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: i1, i2, k1, k2
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r8), intent(out) :: f(k1,k2)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_array_r8
 
     subroutine EZspline_derivative2_cloud_r8(spline_o, i1, i2, k, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: i1, i2, k
       real(ezspline_r8), intent(in) :: p1(k), p2(k)
       real(ezspline_r8), intent(out) :: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_cloud_r8
 
     subroutine EZspline_derivative1_r8(spline_o, i1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(in) :: i1
       real(ezspline_r8), intent(in) :: p1
       real(ezspline_r8), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative1_r8
 
     subroutine EZspline_derivative1_array_r8(spline_o, i1, k1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(in) :: i1, k1
       real(ezspline_r8), intent(in) :: p1(k1)
       real(ezspline_r8), intent(out) :: f(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative1_array_r8
 
     subroutine EZspline_derivative3_r4(spline_o, i1, i2, i3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: i1, i2, i3
       real(ezspline_r4), intent(in) :: p1, p2, p3
       real(ezspline_r4), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_r4
 
     subroutine EZspline_derivative3_array_r4(spline_o, i1, i2, i3, &
          & k1, k2, k3, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: i1, i2, i3, k1, k2, k3
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r4), intent(out) :: f(k1, k2, *)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_array_r4
 
     subroutine EZspline_derivative3_cloud_r4(spline_o, i1, i2, i3, &
          & k, p1, p2, p3, f, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: i1, i2, i3, k
       real(ezspline_r4), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r4), intent(out) :: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative3_cloud_r4
 
     subroutine EZspline_derivative2_r4(spline_o, i1, i2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: i1, i2
       real(ezspline_r4), intent(in) :: p1, p2
       real(ezspline_r4), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_r4
 
     subroutine EZspline_derivative2_array_r4(spline_o, i1, i2, k1, k2, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: i1, i2, k1, k2
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r4), intent(out) :: f(k1,k2)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_array_r4
 
     subroutine EZspline_derivative2_cloud_r4(spline_o, i1, i2, k, p1, p2, f, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: i1, i2, k
       real(ezspline_r4), intent(in) :: p1(k), p2(k)
       real(ezspline_r4), intent(out) :: f(k)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative2_cloud_r4
 
     subroutine EZspline_derivative1_r4(spline_o, i1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(in) :: i1
       real(ezspline_r4), intent(in) :: p1
       real(ezspline_r4), intent(out) :: f
       integer, intent(out) :: ier
     end subroutine EZspline_derivative1_r4
 
     subroutine EZspline_derivative1_array_r4(spline_o, i1, k1, p1, f, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(in) :: i1, k1
       real(ezspline_r4), intent(in) :: p1(k1)
       real(ezspline_r4), intent(out) :: f(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_derivative1_array_r4
 
  end interface
 
  interface EZspline_gradient
     !
     ! Return the gradient in df. When the dimensionality is 1 then
     ! df is df/dx. In more than one dimension, df has rank >=1 with
     ! the last index yielding df/dx, df/dy ... etc. Subsequent indices
     ! reflect the node positions when cloud or array evaluation is
     ! sought, as in df(k1, k2, k3, 1) for df/dx at x(k1), y(k2), z(k3).
     !
     subroutine EZspline_gradient3_r8(spline_o, p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       real(ezspline_r8), intent(in) :: p1, p2, p3
       real(ezspline_r8), intent(out) :: df(3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_r8
 
     subroutine EZspline_gradient3_array_r8(spline_o, k1, k2, k3, &
          & p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r8), intent(out) :: df(k1,k2,k3,3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_array_r8
 
     subroutine EZspline_gradient3_cloud_r8(spline_o, k, &
          & p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r8), intent(out) :: df(k,3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_cloud_r8
 
     subroutine EZspline_gradient2_r8(spline_o, p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       real(ezspline_r8), intent(in) :: p1, p2
       real(ezspline_r8), intent(out) :: df(2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_r8
 
     subroutine EZspline_gradient2_array_r8(spline_o, k1, k2, &
          & p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in)  :: k1, k2
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r8), intent(out) :: df(k1, k2, 2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_array_r8
 
     subroutine EZspline_gradient2_cloud_r8(spline_o, k, &
          & p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in)  :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k)
       real(ezspline_r8), intent(out) :: df(k, 2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_cloud_r8
 
     subroutine EZspline_gradient1_r8(spline_o, p1, df, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       real(ezspline_r8), intent(in) :: p1
       real(ezspline_r8), intent(out) :: df
       integer, intent(out) :: ier
     end subroutine EZspline_gradient1_r8
 
     subroutine EZspline_gradient1_array_r8(spline_o, k1, p1, df, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       integer, intent(in) :: k1
       real(ezspline_r8), intent(in) :: p1(k1)
       real(ezspline_r8), intent(out) :: df(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient1_array_r8
 
     subroutine EZspline_gradient3_r4(spline_o, p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       real(ezspline_r4), intent(in) :: p1, p2, p3
       real(ezspline_r4), intent(out) :: df(3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_r4
 
     subroutine EZspline_gradient3_array_r4(spline_o, k1, k2, k3, &
          & p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2), p3(k3)
       real(ezspline_r4), intent(out) :: df(k1,k2,k3,3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_array_r4
 
     subroutine EZspline_gradient3_cloud_r4(spline_o, k, &
          & p1, p2, p3, df, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k), p3(k)
       real(ezspline_r4), intent(out) :: df(k,3)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient3_cloud_r4
 
     subroutine EZspline_gradient2_r4(spline_o, p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       real(ezspline_r4), intent(in) :: p1, p2
       real(ezspline_r4), intent(out) :: df(2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_r4
 
     subroutine EZspline_gradient2_array_r4(spline_o, k1, k2, &
          & p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in)  :: k1, k2
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2)
       real(ezspline_r4), intent(out) :: df(k1, k2, 2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_array_r4
 
     subroutine EZspline_gradient2_cloud_r4(spline_o, k, &
          & p1, p2, df, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in)  :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k)
       real(ezspline_r4), intent(out) :: df(k, 2)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient2_cloud_r4
 
     subroutine EZspline_gradient1_r4(spline_o, p1, df, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       real(ezspline_r4), intent(in) :: p1
       real(ezspline_r4), intent(out) :: df
       integer, intent(out) :: ier
     end subroutine EZspline_gradient1_r4
 
     subroutine EZspline_gradient1_array_r4(spline_o, k1, p1, df, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       integer, intent(in) :: k1
       real(ezspline_r4), intent(in) :: p1(k1)
       real(ezspline_r4), intent(out) :: df(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_gradient1_array_r4
 
  end interface
 
  interface EZspline_isInDomain
     !
     ! Return error code if position (p1, [p2, [p3]]) is outside domain.
     ! The evaluation can be sought at a single point (p1, [p2, [p3]]
     ! are scalars), on an unordered list of points (p1, [p2, [p3]] have
     ! dimension k), or on a structured grid (p1, [p2, [p3]] have dimension
     ! k1, [k2, [k3]] respectively).
     !
     subroutine EZspline_isInDomain3_r8(spline_o, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) :: spline_o
       real(ezspline_r8), intent(in) :: p1, p2, p3
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_r8
 
     subroutine EZspline_isInDomain3_array_r8(spline_o, k1, k2, k3, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) :: spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2), p3(k3)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_array_r8
 
     subroutine EZspline_isInDomain3_cloud_r8(spline_o, k, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) :: spline_o
       integer, intent(in) :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k), p3(k)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_cloud_r8
 
     subroutine EZspline_isInDomain2_r8(spline_o, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) :: spline_o
       real(ezspline_r8), intent(in) :: p1, p2
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_r8
 
     subroutine EZspline_isInDomain2_array_r8(spline_o, k1, k2, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) :: spline_o
       integer, intent(in) :: k1, k2
       real(ezspline_r8), intent(in) :: p1(k1), p2(k2)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_array_r8
 
     subroutine EZspline_isInDomain2_cloud_r8(spline_o, k, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) :: spline_o
       integer, intent(in) :: k
       real(ezspline_r8), intent(in) :: p1(k), p2(k)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_cloud_r8
 
     subroutine EZspline_isInDomain1_r8(spline_o, p1, ier)
       use EZspline_obj
       type(EZspline1_r8) :: spline_o
       real(ezspline_r8), intent(in) :: p1
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain1_r8
 
     subroutine EZspline_isInDomain1_array_r8(spline_o, k1, p1, ier)
       use EZspline_obj
       type(EZspline1_r8) :: spline_o
       integer, intent(in) :: k1
       real(ezspline_r8), intent(in) :: p1(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain1_array_r8
 
     subroutine EZspline_isInDomain3_r4(spline_o, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) :: spline_o
       real(ezspline_r4), intent(in) :: p1, p2, p3
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_r4
 
     subroutine EZspline_isInDomain3_array_r4(spline_o, k1, k2, k3, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) :: spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2), p3(k3)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_array_r4
 
     subroutine EZspline_isInDomain3_cloud_r4(spline_o, k, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) :: spline_o
       integer, intent(in) :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k), p3(k)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain3_cloud_r4
 
     subroutine EZspline_isInDomain2_r4(spline_o, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) :: spline_o
       real(ezspline_r4), intent(in) :: p1, p2
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_r4
 
     subroutine EZspline_isInDomain2_array_r4(spline_o, k1, k2, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) :: spline_o
       integer, intent(in) :: k1, k2
       real(ezspline_r4), intent(in) :: p1(k1), p2(k2)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_array_r4
 
     subroutine EZspline_isInDomain2_cloud_r4(spline_o, k, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) :: spline_o
       integer, intent(in) :: k
       real(ezspline_r4), intent(in) :: p1(k), p2(k)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain2_cloud_r4
 
     subroutine EZspline_isInDomain1_r4(spline_o, p1, ier)
       use EZspline_obj
       type(EZspline1_r4) :: spline_o
       real(ezspline_r4), intent(in) :: p1
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain1_r4
 
     subroutine EZspline_isInDomain1_array_r4(spline_o, k1, p1, ier)
       use EZspline_obj
       type(EZspline1_r4) :: spline_o
       integer, intent(in) :: k1
       real(ezspline_r4), intent(in) :: p1(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_isInDomain1_array_r4
 
  end interface
 
  interface EZspline_isGridRegular
     !
     ! Return error code if grid is not regular (not strictly  increasing).
     ! The evaluation can be sought at a single point (p1, [p2, [p3]]
     ! are scalars), on an unordered list of points (p1, [p2, [p3]] have
     ! dimension k), or on a structured grid (p1, [p2, [p3]] have dimension
     ! k1, [k2, [k3]] respectively).
     !
     subroutine EZspline_isGridRegular3_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline3_r8) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular3_r8
 
     subroutine EZspline_isGridRegular2_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline2_r8) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular2_r8
 
     subroutine EZspline_isGridRegular1_r8(spline_o, ier)
       use EZspline_obj
       type(EZspline1_r8) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular1_r8
 
     subroutine EZspline_isGridRegular3_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline3_r4) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular3_r4
 
     subroutine EZspline_isGridRegular2_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline2_r4) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular2_r4
 
     subroutine EZspline_isGridRegular1_r4(spline_o, ier)
       use EZspline_obj
       type(EZspline1_r4) :: spline_o
       integer, intent(out) :: ier
     end subroutine EZspline_isGridRegular1_r4
 
  end interface
 
  interface EZspline_save
     !
     ! Save spline/Akima Hermite/Linear object in netcdf file 'filename'. Use
     ! EZspline_load to load spline/Akima Hermite/Linear object from netcdf 
     ! file.
     !
     ! Mod DMC March 2006 -- optionally, by giving each spline a name, 
     ! multiple splines can be saved in a single file.  Also, by specifying
     ! "fullsave=.TRUE." the spline coefficients can be saved as well in the
     ! file, so that they do not have to be recomputed at ezspline_load time.
     !
     ! When creating a single file with multiple spline objects, it is the
     ! user's responsibilitly to make sure that a different name is used for
     ! each spline that is to be saved.  Names can consist of upper or lower
     ! case letters, numerals, and "_", but must not start with a numeral.
     ! Imbedded blanks are not allowed, and the length of the name must be
     ! no more than 20 characters long-- to allow the names of the spline
     ! object elements to be appended.
     !
     subroutine EZspline_save3_r8(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline3_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save3_r8
 
     subroutine EZspline_save2_r8(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline2_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save2_r8
 
     subroutine EZspline_save1_r8(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline1_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save1_r8
 
     subroutine EZspline_save3_r4(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline3_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save3_r4
 
     subroutine EZspline_save2_r4(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline2_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save2_r4
 
     subroutine EZspline_save1_r4(spline_o, filename, ier, &
          spl_name,fullsave)
       use EZspline_obj
       use EZcdf
       type(EZspline1_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
       logical, intent(in), optional :: fullsave

     end subroutine EZspline_save1_r4
 
  end interface
 
  interface EZspline_load
     !
     ! Load spline/Akima Hermite object from netcdf file 'filename'. Use
     ! EZspline_save to save spline/Akima Hermite/Linear object in netcdf file.
     !
     ! MOD DMC March 2006-- a single NetCDF file can now contain multiple
     ! *named* spline objects.  If accessing such an object, the name must
     ! be supplied via the optional argument "spl_name".  If this is omitted,
     ! the default is to read the contents of the file which is presumed to
     ! consist of but a single spline object.

     ! Each call still opens and closes the file; if users find this 
     ! inefficient, an improvement to the control interface may be built.

     subroutine EZspline_load3_r8(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline3_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load3_r8
 
     subroutine EZspline_load2_r8(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline2_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load2_r8
 
     subroutine EZspline_load1_r8(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline1_r8) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load1_r8
 
     subroutine EZspline_load3_r4(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline3_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load3_r4
 
     subroutine EZspline_load2_r4(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline2_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load2_r4
 
     subroutine EZspline_load1_r4(spline_o, filename, ier, spl_name)
       use EZspline_obj
       use EZcdf
       type(EZspline1_r4) :: spline_o
       character*(*) :: filename
       integer, intent(out) :: ier

       character*(*), intent(in),optional :: spl_name
     end subroutine EZspline_load1_r4
  end interface
 
  interface EZspline_modulo
     !
     ! Map argument to (x1,2,3min, x1,2,3max) interval. This is useful to avoid
     ! an out-of-grid error when periodic boundary conditions are applied.
     ! The evaluation can be sought at a single point (p1, [p2, [p3]]
     ! are scalars), on an unordered list of points (p1, [p2, [p3]] have
     ! dimension k), or on a structured grid (p1, [p2, [p3]] have dimension
     ! k1, [k2, [k3]] respectively).
     !
     subroutine EZspline_modulo3_r8(spline_o, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       real(ezspline_r8) :: p1, p2, p3
       integer, intent(out) :: ier
     end subroutine EZspline_modulo3_r8
 
     subroutine EZspline_modulo_array3_r8(spline_o, k1, k2, k3, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r8) :: p1(k1), p2(k2), p3(k3)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array3_r8
 
     subroutine EZspline_modulo_cloud3_r8(spline_o, k, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k
       real(ezspline_r8) :: p1(k), p2(k), p3(k)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_cloud3_r8
 
     subroutine EZspline_modulo2_r8(spline_o, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       real(ezspline_r8) :: p1, p2
       integer, intent(out) :: ier
     end subroutine EZspline_modulo2_r8
 
     subroutine EZspline_modulo_array2_r8(spline_o, k1, k2, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: k1, k2
       real(ezspline_r8) :: p1(k1), p2(k2)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array2_r8
 
     subroutine EZspline_modulo_cloud2_r8(spline_o, k, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r8) spline_o
       integer, intent(in) :: k
       real(ezspline_r8) :: p1(k), p2(k)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_cloud2_r8
 
     subroutine EZspline_modulo1_r8(spline_o, p1, ier)
       use EZspline_obj
       type(EZspline1_r8) spline_o
       real(ezspline_r8) :: p1
       integer, intent(out) :: ier
     end subroutine EZspline_modulo1_r8
 
     subroutine EZspline_modulo_array1_r8(spline_o, k1, p1, ier)
       use EZspline_obj
       type(EZspline3_r8) spline_o
       integer, intent(in) :: k1
       real(ezspline_r8) :: p1(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array1_r8
 
     subroutine EZspline_modulo3_r4(spline_o, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       real(ezspline_r4) :: p1, p2, p3
       integer, intent(out) :: ier
     end subroutine EZspline_modulo3_r4
 
     subroutine EZspline_modulo_array3_r4(spline_o, k1, k2, k3, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k1, k2, k3
       real(ezspline_r4) :: p1(k1), p2(k2), p3(k3)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array3_r4
 
     subroutine EZspline_modulo_cloud3_r4(spline_o, k, p1, p2, p3, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k
       real(ezspline_r4) :: p1(k), p2(k), p3(k)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_cloud3_r4
 
     subroutine EZspline_modulo2_r4(spline_o, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       real(ezspline_r4) :: p1, p2
       integer, intent(out) :: ier
     end subroutine EZspline_modulo2_r4
 
     subroutine EZspline_modulo_array2_r4(spline_o, k1, k2, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: k1, k2
       real(ezspline_r4) :: p1(k1), p2(k2)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array2_r4
 
     subroutine EZspline_modulo_cloud2_r4(spline_o, k, p1, p2, ier)
       use EZspline_obj
       type(EZspline2_r4) spline_o
       integer, intent(in) :: k
       real(ezspline_r4) :: p1(k), p2(k)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_cloud2_r4
 
     subroutine EZspline_modulo1_r4(spline_o, p1, ier)
       use EZspline_obj
       type(EZspline1_r4) spline_o
       real(ezspline_r4) :: p1
       integer, intent(out) :: ier
     end subroutine EZspline_modulo1_r4
 
     subroutine EZspline_modulo_array1_r4(spline_o, k1, p1, ier)
       use EZspline_obj
       type(EZspline3_r4) spline_o
       integer, intent(in) :: k1
       real(ezspline_r4) :: p1(k1)
       integer, intent(out) :: ier
     end subroutine EZspline_modulo_array1_r4
 
  end interface
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EZspline object-less methods (first argument is NOT an EZspline1,2,3 type).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  interface EZspline_2NetCDF
     !
     ! Save data in netCDF file 'filename'. To save an EZspline1,2,3 object use
     ! EZspline_save method.
     !
     subroutine EZspline_2NetCDF_array3_r8(n1, n2, n3, x1, x2, x3, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1, n2, n3
       real(ezspline_r8), intent(in) :: x1(:), x2(:), x3(:), f(:, :, :)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF_array3_r8
 
     subroutine EZspline_2NetCDF_array2_r8(n1, n2, x1, x2, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1, n2
       real(ezspline_r8), intent(in) ::  x1(:), x2(:), f(:,:)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF_array2_r8
 
     subroutine EZspline_2NetCDF1_r8(n1, x1, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1
       real(ezspline_r8), intent(in) :: x1(:), f(:)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF1_r8
 
   subroutine EZspline_2NetCDF_cloud3_r8(n, x1, x2, x3, f, filename, ier)
     use EZspline_obj
     use EZcdf
     implicit none
     integer, intent(in) :: n
     real(ezspline_r8), intent(in) :: x1(:), x2(:), x3(:), f(:)
     character*(*), intent(in) :: filename
     integer, intent(out) :: ier
   end subroutine EZspline_2NetCDF_cloud3_r8
 
   subroutine EZspline_2NetCDF_cloud2_r8(n, x1, x2, f, filename, ier)
     use EZspline_obj
     use EZcdf
     implicit none
     integer, intent(in) :: n
     real(ezspline_r8), intent(in) :: x1(:), x2(:), f(:)
     character*(*), intent(in) :: filename
     integer, intent(out) :: ier
   end subroutine EZspline_2NetCDF_cloud2_r8
 
     subroutine EZspline_2NetCDF_array3_r4(n1, n2, n3, x1, x2, x3, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1, n2, n3
       real(ezspline_r4), intent(in) :: x1(:), x2(:), x3(:), f(:, :, :)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF_array3_r4
 
     subroutine EZspline_2NetCDF_array2_r4(n1, n2, x1, x2, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1, n2
       real(ezspline_r4), intent(in) ::  x1(:), x2(:), f(:,:)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF_array2_r4
 
     subroutine EZspline_2NetCDF1_r4(n1, x1, f, filename, ier)
       use EZspline_obj
       use EZcdf
       implicit none
       integer, intent(in) :: n1
       real(ezspline_r4), intent(in) :: x1(:), f(:)
       character*(*), intent(in) :: filename
       integer, intent(out) :: ier
     end subroutine EZspline_2NetCDF1_r4
 
   subroutine EZspline_2NetCDF_cloud3_r4(n, x1, x2, x3, f, filename, ier)
     use EZspline_obj
     use EZcdf
     implicit none
     integer, intent(in) :: n
     real(ezspline_r4), intent(in) :: x1(:), x2(:), x3(:), f(:)
     character*(*), intent(in) :: filename
     integer, intent(out) :: ier
   end subroutine EZspline_2NetCDF_cloud3_r4
 
   subroutine EZspline_2NetCDF_cloud2_r4(n, x1, x2, f, filename, ier)
     use EZspline_obj
     use EZcdf
     implicit none
     integer, intent(in) :: n
     real(ezspline_r4), intent(in) :: x1(:), x2(:), f(:)
     character*(*), intent(in) :: filename
     integer, intent(out) :: ier
   end subroutine EZspline_2NetCDF_cloud2_r4
 
  end interface
 
 
contains
 
 
  subroutine EZspline_error(ier)
    !
    ! Error handling routine. Maps error ier code to a meaningful message.
    ! Note: does not abort nor stop if ier/=0.
    !
    implicit none
    integer, intent(in) :: ier
 
    if(ier == 0) return
    print*,'**EZspline** ERROR/WARNING #', ier,' occurred'
 
    select case(ier)
    case(1)
       print*,'**EZspline** allocation error'
    case(2)
       print*,'**EZspline** wrong BCS1 code'
    case(3)
       print*,'**EZspline** wrong BCS2 code'
    case(4)
       print*,'**EZspline** wrong BCS3 code'
    case(5)
       print*,'**EZspline** Que??'
    case(6)
       print*,'**EZspline** out of interval p1 < min(x1)'
    case(7)
       print*,'**EZspline** out of interval p1 > max(x1)'
    case(8)
       print*,'**EZspline** out of interval p2 < min(x2)'
    case(9)
       print*,'**EZspline** out of interval p2 > max(x2)'
    case(10)
       print*,'**EZspline** out of interval p3 < min(x3)'
    case(11)
       print*,'**EZspline** out of interval p3 > max(x3)'
    case(12)
       print*,'**EZspline** negative derivative order'
    case(13)
       print*,'**EZspline** derivative order too high'
    case(14)
       print*,'**EZspline** x1 grid is not strictly increasing'
    case(15)
       print*,'**EZspline** x2 grid is not strictly increasing'
    case(16)
       print*,'**EZspline** x3 grid is not strictly increasing'
    case(17)
       print*,'**EZspline** could not save spline object in file '
    case(18)
       print*,'**EZspline** memory allocation failure in coefficient setup'
 
    case(20)
       print*,'**EZspline** attempt to load spline object with wrong rank.'
    case(21)
       print*,'**EZspline** could not load spline object from file '
    case(22)
       print*,'**EZspline** loaded spline object from file but failed at coefficient set-up'
    case(23)
       print*,'**EZspline** failed to free spline object'
    case(24)
       print*,'**EZspline** 2nd order derivative not supported for Akima-Hermite (isHermite=1)'
    case(25)
       print*,'**EZspline** not supported for Akima-Hermite (isHermite=1)'
    case(26)
       print*,'**EZspline** memory allocation error in EZspline_interp'
    case(27)
       print*,'**EZspline** an error ocurred in genxpkg'
    case(28)
       print*,'**EZspline** memory allocation failure in ezspline_interp'
    case(29)
       print*,'**EZspline** memory deallocation failure in ezspline_interp'
    case(30)
       print*,'**EZspline** memory allocation error in EZspline_gradient'
    case(31)
       print*,'**EZspline** memory deallocation error in EZspline_gradient'
    case(32)
       print*,'**EZspline** memory allocation error in EZspline_derivative'
    case(33)
       print*,'**EZspline** memory deallocation error in EZspline_derivative'
    case(34)
       print*,'**EZspline** could not open netCDF file in EZspline_2netcdf'
    case(35)
       print*,'**EZspline** could not write into netCDF file in EZspline_2netcdf'
    case(36)
       print*,'**EZspline** could not read from netCDF file in EZspline_2netcdf'
    case(37)
       print*,'**EZspline** could not close netCDF file in EZspline_2netcdf'
    case(38)
       print*,'**EZspline** could not define variable (cdfDefVar) in EZspline_2netcdf'
    case(39)
       print*,'**EZspline** could not open netCDF file in EZspline_save'
    case(40)
       print*,'**EZspline** could not write into netCDF file in EZspline_save'
    case(41)
       print*,'**EZspline** could not close netCDF file in EZspline_save'
    case(42)
       print*,'**EZspline** could not define variable (cdfDefVar) in EZspline_save'
    case(43)
       print*,'**EZspline** could not open netCDF file in EZspline_load'
    case(44)
       print*,'**EZspline** could not read from netCDF file in EZspline_load'
    case(45)
       print*,'**EZspline** could not close netCDF file in EZspline_load'
    case(46)
       print*,'**EZspline** 2nd order derivative not supported for Piecewise Linear Interpolation (isLinear=1)'
    case(47)
       print*,'**EZspline** not supported for Piecewise Linear Interpolation (isLinear=1)'

    case(50)
       print*,'**EZspline** ezspline_save (optional) spline name is blank.'
    case(51)
       print*,'**EZspline** ezspline_save (optional) spline name too long (max 20 characters).'
    case(52)
       print*,'**EZspline** ezspline_save (optional) spline name contains'
       print*,'             imbedded blanks or other illegal characters.'
    case(53)
       print*,'**EZspline** attempt to write named spline object to NetCDF'
       print*,'             file with change of dimensionality or data type.'
       
    case(54)
       print*,'**EZspline** hybrid interpolation specification not in range -1:2'
       print*,'             error in EZhybrid_init.'
    case(55)
       print*,'**EZspline** hybrid interpolation cannot mix Hermite and Spline interpolation.'
       print*,'             hspline(i)=1 and hspline(j)=2 in EZhybrid_init.'
    case(56)
       print*,'**EZspline** non-default boundary condition unavailable: zonal or piecewise linear dimension.'
       print*,'             in EZhybrid_init.'

    case(57)
       print*,'**EZspline** dimension of "f" smaller than corresponding "fspl"'
       print*,'             dimension in "spline_o".'
    case(58)
       print*,'**EZspline** dimension of "f" larger than corresponding "fspl"'
       print*,'             dimension in "spline_o".'

    case(90)
       print*,'**EZspline** an error occurred after attempting to evaluate the'
       print*,'             Hermite polynomials'
    case(91)
       print*,'**EZspline** an error occurred after attempting to set up the'
       print*,'             Hermite polynomial coefficients'
    case(92)
       print*,'**EZspline** warning in EZspline_load. Looks like saved object '
       print*,'             was not properly set-up (isReady=0).'
    case(93)
       print*,'**EZspline** warning in EZspline_save. Looks like saved object '
       print*,'             was not properly set-up (isReady=0).'
    case(94)
       print*,'**EZspline** an error occurred in EZspline_interp. Did you forget'
       print*,'             to set up the cubic spline coefficients by calling'
       print*,'             call EZspline_setup(spline_o, f, ier)'
       print*,'             ?'
    case(95)
       print*,'**EZspline** some error occurred in EZspline_gradient'
    case(96)
       print*,'**EZspline** some error occurred in EZspline_derivative'
    case(97)
       print*,'**EZspline** some error occurred in EZspline_interp apparently'
       print*,'             related to a PSPLINE routine. Check if argument is '
       print*,'             outside interpolation domain by calling'
       print*,'             call EZspline_isInDomain(spline_o, [[k1, k2, k3,] .OR. k,] p1, p2, p3, ier ,ier)'
       print*,'             call EZspline_error(ier)'
    case(98)
       print*,'**EZspline** error occurred in EZspline_setup'
       print*,'  if no other explanation-- ezspline_init call never made.'
    case(99)
       print*,'**EZspline** some error occurred in EZspline_init, EZhybrid_init,  or EZlinear_init'
    case(100)
       print*,'**EZSPLINE** EZspline_init, EZhybrid_init,  or EZlinear_init -- object already allocated.'
    case(101)
       print*,'**EZSPLINE** object was never allocated.'
    case default
       print*,'**EZspline** '
    end select
 
    return
  end subroutine EZspline_error
 
 

end module EZspline
