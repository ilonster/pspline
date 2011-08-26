      subroutine cstring(fstr,cstr,cmd)
C
C  Fortran-77 <-> C string converter
C
C  usage:  call cstring('this is a string',cstr,'2C')
C          ...fills the byte array cstr with a null terminated string
C             suitable for being passed to a C routine
C
C               **caution**
C               in  "call cstring(fstr,cstr,'2C')" trailing blanks in
C               fstr are also copied; if this is not desired use something
C               like
C                     ilstr=max(1,str_length(fstr))
C                     call cstring(fstr(1:ilstr),cstr,'2C')
C
C
C          call cstring(fstr,cstr,'2F') copies the contents of the
C                byte array cstr into fstr, stopping when a terminating
C                null is encountered or the end of fstr is reached,
C                whichever happens first.  fstr is padded with blanks.
C
C
      implicit none
C
#include "fpreproc/byte_declare.h"
 
C
C  fortran string:
C
      character*(*) fstr
C
C  C string (null terminated byte array)
C
      byte_declare cstr(*)
C
C  cmd string:  "2F" for cstr->fstr copy, "2C" for fstr->cstr copy.
C
      character*2 cmd
C
C--------------------------
C  local:
C
      integer i,ilen
C------------------------------------------------
C
      if((cmd.ne.'2C').and.(cmd.ne.'2F')) then
         call errmsg_exit(' ??cstring:  invalid cmd value:  '//cmd)
         call bad_exit
      endif
C
      if(cmd.eq.'2F') then
         fstr=' '
         do i=1,len(fstr)
#if __CRAY || __SX
            if(cstr(i).eq. char(0)) go to 10   ! stop at terminating null
            fstr(i:i)=cstr(i)
#else
            if(cstr(i).eq.0) go to 10   ! stop at terminating null
            fstr(i:i)=char(cstr(i))
#endif
         enddo
 10      continue
      endif
C
      if(cmd.eq.'2C') then
         ilen=len(fstr)
         do i=1,ilen
#if __CRAY || __SX
            cstr(i)=fstr(i:i)
         enddo
            cstr(ilen+1)=char(0)
#else
            cstr(i)=ichar(fstr(i:i))
         enddo
         cstr(ilen+1)=0
#endif
      endif
C
      return
      end
 
 
 
 
