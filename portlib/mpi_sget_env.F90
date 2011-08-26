subroutine cmpi_sget_env(cname_in,cvalue_out,ierr)

  ! C callable mpi_sget_env

  use mpi_env_mod
  implicit NONE

#include "fpreproc/byte_declare.h"

  BYTE_DECLARE cname_in(*), cvalue_out(*)
  integer, intent(out) :: ierr

  !-------------------------------

  character*(maxlen_name) :: name_in
  character*(maxlen_val) :: value

  !-------------------------------

  call cstring(name_in,cname_in,'2F')  ! cname_in -> name_in

  call mpi_sget_env(name_in,value,ierr)

  call cstring(trim(value),cvalue_out,'2C')  ! value -> cvalue_out

end subroutine cmpi_sget_env

subroutine mpi_sget_env(name_in,value,ierr)

  use mpi_env_mod
  implicit NONE

  !  return value associated with name_in:
  !    1st look in mpi_env_mod; if not found there, from OS environment
  !    in which case value found from OS environment is stored in the module.

  character*(*), intent(in) :: name_in
  character*(*), intent(out) :: value
  integer, intent(out) :: ierr

  !--------------------------------------------
  integer :: ilen,iadr
  logical :: inew
  !--------------------------------------------

  call legal_name(name_in,ilen,ierr)
  if(ierr.ne.0) return

  call lookup_alloc(name_in,iadr,inew)

  if(.not.inew) then
     ! use module value

     value = mpi_env_vals(iadr)

  else
     ! get from OS environment; load module value
     mpi_env_names(iadr)=name_in
     call sget_env(name_in,value)
     mpi_env_vals(iadr) = value

  endif

end subroutine mpi_sget_env
