      subroutine frename(oldname,newname,ier)
C
C  dmc 18 June 1996 -- rename file
C
C  encapsulate VMS/UNIX OS dependence here.
C
      character*(*) oldname   ! old filename
      character*(*) newname   ! new filename
C
C  ** rename a file **
C
C  return error code
C    ier=0 -- success
C    ier=1 -- error
C
      integer str_length
C
#ifdef __UNIX
#include "fpreproc/byte_declare.h"
      byte_declare fnbuf1(500),fnbuf2(500)
      integer cdelete,crename
#endif
C----------------------------------
C
      ier=0
C
      ilo=str_length(oldname)
      iln=str_length(newname)
      if(min(ilo,iln).le.0) then
         write(6,*) ' ?frename:  filename argument is blank.'
         ier=1
         return
      endif
C
C  UNIX code:
C
#ifdef __UNIX
      call cstring(oldname(1:ilo),fnbuf1,'2C')
      call cstring(newname(1:iln),fnbuf2,'2C')
      ier=cdelete(fnbuf2)
      ier=crename(fnbuf1,fnbuf2)
#endif
C
C  VMS code:
C
#ifdef __VMS
      istat=lib$rename_file(oldname(1:ilo),newname(1:iln))
      if(istat.ne.1) then
        ier=1
      else
        ier=0
      endif
#endif
C
      return
      end
