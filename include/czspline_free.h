! -*-f90-*-
!---------------------------------------------------------------------------
! This code was developed at Tech-X (www.txcorp.com). It is free for any one
! to use but comes with no warranty whatsoever. Use at your own risk. 
! Thanks for reporting bugs to pletzer@txcorp.com. 
!---------------------------------------------------------------------------
subroutine _S(handle, ier)
   use ezspline_obj
   use ezspline
   use czspline_pointer_types
   implicit none
   integer, intent(inout) :: handle(_ARRSZ)
   integer, intent(out) :: ier
   type(_O) :: self
   self = transfer(handle, self)
   call ezspline_free(self %ptr, ier)
   deallocate(self %ptr)
   handle = 0
end subroutine _S
