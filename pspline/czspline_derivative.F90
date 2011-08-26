! -*-f90-*-
! $Id: czspline_derivative.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Compute derivatives

#include "czspline_handle_size.h"

! point
#define _S czspline_derivative1_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, i1, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   real(_P), intent(in) :: p1
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative1_cloud_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, i1, k, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, k, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative1_array_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, i1, k1, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   integer, intent(in) :: k1
   real(_P), intent(in) :: p1(k1)
   real(_P), intent(out) :: f(k1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, k1, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! point
#define _S czspline_derivative2_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   real(_P), intent(in) :: p1, p2
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative2_cloud_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, k, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k), p2(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, k, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative2_array_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, k1, k2, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   integer, intent(in) :: k1, k2
   real(_P), intent(in) :: p1(k1), p2(k2)
   real(_P), intent(out) :: f(k1, k2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, k1, k2, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! point
#define _S czspline_derivative3_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, i3, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   real(_P), intent(in) :: p1, p2, p3
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative3_cloud_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, i3, k, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k), p2(k), p3(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, k, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative3_array_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, i1, i2, i3, k1, k2, k3, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   integer, intent(in) :: k1, k2, k3
   real(_P), intent(in) :: p1(k1), p2(k2), p3(k3)
   real(_P), intent(out) :: f(k1, k2, k3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, k1, k2, k3, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! point
#define _S czspline_derivative1_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, i1, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   real(_P), intent(in) :: p1
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative1_cloud_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, i1, k, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, k, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative1_array_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, i1, k1, p1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1
   integer, intent(in) :: k1
   real(_P), intent(in) :: p1(k1)
   real(_P), intent(out) :: f(k1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, k1, p1, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! point
#define _S czspline_derivative2_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   real(_P), intent(in) :: p1, p2
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative2_cloud_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, k, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k), p2(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, k, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative2_array_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, k1, k2, p1, p2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2
   integer, intent(in) :: k1, k2
   real(_P), intent(in) :: p1(k1), p2(k2)
   real(_P), intent(out) :: f(k1, k2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, k1, k2, p1, p2, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! point
#define _S czspline_derivative3_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, i3, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   real(_P), intent(in) :: p1, p2, p3
   real(_P), intent(out) :: f
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

! cloud
#define _S czspline_derivative3_cloud_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, i3, k, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   integer, intent(in) :: k
   real(_P), intent(in) :: p1(k), p2(k), p3(k)
   real(_P), intent(out) :: f(k)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, k, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

!array
#define _S czspline_derivative3_array_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, i1, i2, i3, k1, k2, k3, p1, p2, p3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: i1, i2, i3
   integer, intent(in) :: k1, k2, k3
   real(_P), intent(in) :: p1(k1), p2(k2), p3(k3)
   real(_P), intent(out) :: f(k1, k2, k3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_derivative(self % ptr, i1, i2, i3, k1, k2, k3, p1, p2, p3, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

