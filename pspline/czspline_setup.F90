! -*-f90-*-
! $Id: czspline_setup.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Compute spline coefficients

#include "czspline_handle_size.h"

#define _S czspline_setup1_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, n1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1
   real(_P), intent(in) :: f(n1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_setup2_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, n1, n2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2
   real(_P), intent(in) :: f(n1, n2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_setup3_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, n1, n2, n3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2, n3
   real(_P), intent(in) :: f(n1, n2, n3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_setup1_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, n1, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1
   real(_P), intent(in) :: f(n1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_setup2_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, n1, n2, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2
   real(_P), intent(in) :: f(n1, n2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_setup3_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, n1, n2, n3, f, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2, n3
   real(_P), intent(in) :: f(n1, n2, n3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_setup(self % ptr, f, ier)
end subroutine _S
#undef _S
#undef _O
#undef _P


