! -*-f90-*-
! $Id: czspline_pointer_types.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Boiler plate code to generate pointer to types for r4/r8 precision and
! 1, 2, and 3 dimensions

module czspline_pointer_types

use ezspline_obj
use ezspline

#define _O1 czspline1_r4
#define _O2 ezspline1_r4
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

#define _O1 czspline2_r4
#define _O2 ezspline2_r4
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

#define _O1 czspline3_r4
#define _O2 ezspline3_r4
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

#define _O1 czspline1_r8
#define _O2 ezspline1_r8
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

#define _O1 czspline2_r8
#define _O2 ezspline2_r8
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

#define _O1 czspline3_r8
#define _O2 ezspline3_r8
! generic implementation
#include "czspline_type.h"
#undef _O1
#undef _O2

end module czspline_pointer_types
