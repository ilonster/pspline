#if  __IBM || __RS6000
  subroutine  flush(unit)
  implicit NONE
  integer unit
  call flush_(unit)
  return
  end
#else
   subroutine dummy_flush(unit)
   implicit NONE
   integer unit
   return
   end  
#endif
