! -*-f90-*-
! $Id: czspline_free.f90,v 1.1 2008/05/22 16:38:14 Alex_Pletzer Exp $
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
! Finalization of ezspline

#include "czspline_handle_size.h"

#define _S czspline_free1_r4
#define _O czspline1_r4
#include "czspline_free.h"
#undef _S
#undef _O

#define _S czspline_free2_r4
#define _O czspline2_r4
#include "czspline_free.h"
#undef _S
#undef _O

#define _S czspline_free3_r4
#define _O czspline3_r4
#include "czspline_free.h"
#undef _S
#undef _O

#define _S czspline_free1_r8
#define _O czspline1_r8
#include "czspline_free.h"
#undef _S
#undef _O

#define _S czspline_free2_r8
#define _O czspline2_r8
#include "czspline_free.h"
#undef _S
#undef _O

#define _S czspline_free3_r8
#define _O czspline3_r8
#include "czspline_free.h"
#undef _S
#undef _O
