subroutine read_word(filename,aword,ier)
 
  !  read a word from a file.  Word is terminated by a blank or null
  !  or EOF or any non-printable character.  No FORTRAN LUN is used.
 
  character*(*), intent(in) :: filename
  character*(*), intent(out) :: aword
  integer, intent(out) :: ier             ! =0: OK, otherwise "C" errno code.
 
  ! --------------------------
 
#include "fpreproc/byte_declare.h"
 
  byte_declare cfname(1+len(filename))
  byte_declare cword(1+len(aword))
 
  integer str_length

  ! --------------------------
 
  call cstring(filename(1:str_length(filename)),cfname,'2C')
 
  call portlib_readword(cfname,cword,len(aword)+1,ier)

  call cstring(aword,cword,'2F')
 
  return
 
end subroutine read_word
