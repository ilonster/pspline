      subroutine sset_env(varname,value,ierr)
C
C  set the value of an environment variable (UNIX)
C
      implicit NONE
C
      character*(*), intent(in) :: varname
      character*(*), intent(in) :: value
      integer, intent(out) :: ierr    ! completion code (0=OK)
C
C-------------------------
#include "fpreproc/byte_declare.h"
C
      integer :: ilenvar,ilenval
C
      byte_declare, dimension(:), allocatable :: cvar,cval
      integer :: f77_setenv
C-------------------------
C
      ilenvar = len(trim(varname))
      ilenval = len(trim(value))
C
      allocate(cvar(ilenvar+1),cval(ilenval+1))  ! leave room for trailing null
C
      call cstring(trim(varname),cvar,'2C')
      call cstring(trim(value),cval,'2C')
C
      ierr = f77_setenv(cvar,cval)
      ierr = abs(ierr)
C
      return
      end
