      subroutine sset_cwd(dir,ierr)
      character*(*) dir                 ! directory to cd to...
      integer ierr                      ! error status code, 0 = normal
 
#include "fpreproc/byte_declare.h"
      byte_declare dirbuf(256)
 
      integer chdir
      integer str_length,ild
C
      ild=max(1,str_length(dir))
#ifdef __VMS
      write(6,*) ' ?sset_cwd:  VMS:  cannot change directory.'
      ierr = 1                          ! VMS:  no can do
#endif
#ifdef __UNIX
 
#ifdef __CRAY
      call pxfchdir(dir,0,ierr)
#elif __HP || __IBM || __RS6000
      ierr = chdir(dir(1:ild)//char(0))
#else
      if(ild.le.255) then
         call cstring(dir(1:ild),dirbuf,'2C')
         ierr = linux_chdir(dirbuf)
      else
         write(6,*) ' ?sset_ccwd:  directory path too long:'
         write(6,*) dir(1:ild)
         ierr=1
      endif
#endif
 
#endif  /*  __UNIX  */
 
      return
      end
 
