      integer function max_reclen(filename)
c
c  return the maximum record length of a (sequential ascii) file.
c  if the file cannot be opened, return (-1)
c
c  use a c subroutine
c
#include "fpreproc/byte_declare.h"
c
      implicit none
c
      character*(*) filename
c
      byte_declare, dimension(:), allocatable :: c_filename
      integer ilen
      integer max_crec
      integer str_length
c
c---------------------
c
      if(filename.eq.' ') then
         max_reclen=-2
         return
      endif
c
      ilen=max(1,str_length(filename))
      allocate(c_filename(ilen+1))
c
c  make null terminated byte string from filename & call c routine
c
      call cstring(filename(1:ilen),c_filename,'2C')
c
      max_reclen=max_crec(c_filename)
c
      deallocate(c_filename)
 
      return
      end
