! -*-f90-*-
! $Id: czspline_setters.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Member setters

#include "czspline_handle_size.h"

! x1, x2, x3
#define _S czspline_set_axes1_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, k1, x1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1
   real(_P), intent(in) :: x1(k1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_axes2_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, k1, k2, x1, x2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1, k2
   real(_P), intent(in) :: x1(k1), x2(k2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
   self % ptr % x2 = x2
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_axes3_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, k1, k2, k3, x1, x2, x3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1, k2, k3
   real(_P), intent(in) :: x1(k1), x2(k2), x3(k3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
   self % ptr % x2 = x2
   self % ptr % x3 = x3
end subroutine _S
#undef _S
#undef _O
#undef _P

! isHermite
#define _S czspline_set_ishermite1_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_ishermite2_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_ishermite3_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

! Boundary conditions bcval1min, bcval1max, ...
#define _S czspline_set_bcvals1_r4
#define _O czspline1_r4
#define _P ezspline_r4
subroutine _S(handle, bcval1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_bcvals2_r4
#define _O czspline2_r4
#define _P ezspline_r4
subroutine _S(handle, bcval1, bcval2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2), bcval2(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
   self % ptr % bcval2min = bcval2(1)
   self % ptr % bcval2max = bcval2(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_bcvals3_r4
#define _O czspline3_r4
#define _P ezspline_r4
subroutine _S(handle, bcval1, bcval2, bcval3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2), bcval2(2), bcval3(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
   self % ptr % bcval2min = bcval2(1)
   self % ptr % bcval2max = bcval2(2)
   self % ptr % bcval3min = bcval3(1)
   self % ptr % bcval3max = bcval3(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

! x1, x2, x3
#define _S czspline_set_axes1_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, k1, x1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1
   real(_P), intent(in) :: x1(k1)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_axes2_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, k1, k2, x1, x2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1, k2
   real(_P), intent(in) :: x1(k1), x2(k2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
   self % ptr % x2 = x2
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_axes3_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, k1, k2, k3, x1, x2, x3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: k1, k2, k3
   real(_P), intent(in) :: x1(k1), x2(k2), x3(k3)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % x1 = x1
   self % ptr % x2 = x2
   self % ptr % x3 = x3
end subroutine _S
#undef _S
#undef _O
#undef _P

! isHermite
#define _S czspline_set_ishermite1_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_ishermite2_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_ishermite3_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, flag, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)  :: flag
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % isHermite = flag
end subroutine _S
#undef _S
#undef _O
#undef _P

! Boundary types
#define _S czspline_set_bctypes1_r8
#define _O czspline1_r8
subroutine _S(handle, bctype1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)    :: bctype1(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % ibctype1 = bctype1
end subroutine _S
#undef _S
#undef _O

#define _S czspline_set_bctypes2_r8
#define _O czspline2_r8
subroutine _S(handle, bctype1, bctype2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)    :: bctype1(2), bctype2(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % ibctype1 = bctype1
   self % ptr % ibctype2 = bctype2
end subroutine _S
#undef _S
#undef _O

#define _S czspline_set_bctypes3_r8
#define _O czspline3_r8
subroutine _S(handle, bctype1, bctype2, bctype3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(in)    :: bctype1(2), bctype2(2), bctype3(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % ibctype1 = bctype1
   self % ptr % ibctype2 = bctype2
   self % ptr % ibctype3 = bctype3
end subroutine _S
#undef _S
#undef _O

! Boundary conditions bcval1min, bcval1max, ...
#define _S czspline_set_bcvals1_r8
#define _O czspline1_r8
#define _P ezspline_r8
subroutine _S(handle, bcval1, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_bcvals2_r8
#define _O czspline2_r8
#define _P ezspline_r8
subroutine _S(handle, bcval1, bcval2, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2), bcval2(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
   self % ptr % bcval2min = bcval2(1)
   self % ptr % bcval2max = bcval2(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

#define _S czspline_set_bcvals3_r8
#define _O czspline3_r8
#define _P ezspline_r8
subroutine _S(handle, bcval1, bcval2, bcval3, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   real(_P), intent(in)   :: bcval1(2), bcval2(2), bcval3(2)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   ier = 0
   self % ptr % bcval1min = bcval1(1)
   self % ptr % bcval1max = bcval1(2)
   self % ptr % bcval2min = bcval2(1)
   self % ptr % bcval2max = bcval2(2)
   self % ptr % bcval3min = bcval3(1)
   self % ptr % bcval3max = bcval3(2)
end subroutine _S
#undef _S
#undef _O
#undef _P

