subroutine mpi_env_comm_get(icomm)

  !  return the mpi_env_mod MPI communicator variable

  use mpi_env_mod
  implicit NONE

  integer, intent(out) :: icomm

  icomm = mpi_env_comm

end subroutine mpi_env_comm_get


subroutine mpi_env_comm_set(icomm)

  !  return the mpi_env_mod MPI communicator variable

  use mpi_env_mod
  implicit NONE

  integer, intent(in) :: icomm

  mpi_env_comm = icomm

end subroutine mpi_env_comm_set


