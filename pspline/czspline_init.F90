! -*-f90-*-
! $Id: czspline_init.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Initialization of ezspline

#include "czspline_handle_size.h"

#define _S czspline_init1_r4
#define _O czspline1_r4
subroutine _S(handle, n1, BCS1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1
   integer, intent(in) :: BCS1(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, BCS1, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O

#define _S czspline_init2_r4
#define _O czspline2_r4
subroutine _S(handle, n1, n2, BCS1, BCS2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2
   integer, intent(in) :: BCS1(2), BCS2(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, n2, BCS1, BCS2, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O

#define _S czspline_init3_r4
#define _O czspline3_r4
subroutine _S(handle, n1, n2, n3, BCS1, BCS2, BCS3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2, n3
   integer, intent(in) :: BCS1(2), BCS2(2), BCS3(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, n2, n3, BCS1, BCS2, BCS3, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O

#define _S czspline_init1_r8
#define _O czspline1_r8
subroutine _S(handle, n1, BCS1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1
   integer, intent(in) :: BCS1(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, BCS1, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O

#define _S czspline_init2_r8
#define _O czspline2_r8
subroutine _S(handle, n1, n2, BCS1, BCS2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2
   integer, intent(in) :: BCS1(2), BCS2(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, n2, BCS1, BCS2, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O

#define _S czspline_init3_r8
#define _O czspline3_r8
subroutine _S(handle, n1, n2, n3, BCS1, BCS2, BCS3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in) :: n1, n2, n3
   integer, intent(in) :: BCS1(2), BCS2(2), BCS3(2)
   integer, intent(out) :: ier
   type(_O) :: self
   allocate(self % ptr)
   call ezspline_init(self % ptr, n1, n2, n3, BCS1, BCS2, BCS3, ier)
   handle = 0
   handle = transfer(self, handle)
end subroutine _S
#undef _S
#undef _O
