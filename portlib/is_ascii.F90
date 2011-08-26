subroutine is_ascii(filename,iascii,ier)
 
  !  determine if a file is ascii
 
  character*(*), intent(in) :: filename
  integer, intent(out) :: iascii          ! =1: ascii, =0: no, =-1: error
  integer, intent(out) :: ier             ! =0: OK, otherwise "C" errno code.
 
  ! --------------------------
 
  integer str_length
 
#include "fpreproc/byte_declare.h"
 
  byte_declare cfname(1+len(filename))
 
  ! --------------------------
 
  call cstring(filename(1:str_length(filename)),cfname,'2C')
 
  call portlib_isascii(cfname,iascii,ier)   ! call the "C" routine to do it...
 
  return
 
end subroutine is_ascii
