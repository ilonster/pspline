      subroutine fdelete(filename,ier)
C
C  dmc 18 June 1996 -- delete file
C
C  encapsulate VMS/UNIX OS dependence here.
C
      character*(*) filename  ! file to delete...
C
C  ** delete a file **
C
C  return error code
C    ier=0 -- success
C    ier=1 -- error
C
C  VMS note:  if no file version number is supplied, ";*" is appended;
C    this will delete all versions of the named file.
C
C
      integer str_length
C
#ifdef __UNIX
#include "fpreproc/byte_declare.h"
      byte_declare fnbuf(500)
      integer cdelete
#endif
C----------------------------------
C
      ier=0
C
      ilf=str_length(filename)
      if(ilf.eq.0) then
         write(6,*) ' ?fdelete:  filename argument is blank.'
         ier=1
         return
      endif
C
C  UNIX code:
C
#ifdef __UNIX
      call cstring(filename(1:ilf),fnbuf,'2C')
      ier=cdelete(fnbuf)
#endif
C
C  VMS code:
C
#ifdef __VMS
      ichk=0
      if(filename(ilf:ilf).eq.'.') ichk=1
      if(index(filename,';').gt.0) ichk=1
      if(ichk.eq.1) then
         istat=lib$delete_file(filename(1:ilf))
      else
         istat=lib$delete_file(filename(1:ilf)//';*')
      endif
      if(istat.ne.1) then
        ier=1
      else
        ier=0
      endif
#endif
C
      return
      end
