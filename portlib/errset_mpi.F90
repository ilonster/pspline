subroutine errset_mpi_enable

  ! ACTIVATE collective error handling

  use mpi_env_mod
  implicit NONE

  mpi_errstat_active = .TRUE.
  mpi_errstat = 0

end subroutine errset_mpi_enable

subroutine errset_mpi_disable

  ! DEACTIVATE collective error handling

  use mpi_env_mod
  implicit NONE

  mpi_errstat_active = .FALSE.
  mpi_errstat = 0

end subroutine errset_mpi_disable

subroutine errset_mpi_query(errflag)

  ! query value of mpi_errstat_active

  use mpi_env_mod
  implicit NONE

  logical, intent(out) :: errflag

  errflag = mpi_errstat_active

end subroutine errset_mpi_query

subroutine errset_mpi_query_int(i_errflag)

  ! query value of mpi_errstat_active: return i_errflag = 1 if set, 0 if not

  use mpi_env_mod
  implicit NONE

  integer, intent(out) :: i_errflag

  if(mpi_errstat_active) then
     i_errflag = 1
  else
     i_errflag = 0
  endif

end subroutine errset_mpi_query_int

subroutine errset_mpi(myid,istat)

  ! set an error status: istat>0 expected & enforced
  ! A CALL TO THIS ROUTINE KILLS YOUR RUN... EVEN IF mpi_errstat_enable=.FALSE.

  use mpi_env_mod
  implicit NONE

  integer, intent(in) :: myid   ! process ID: 0,1,2,...<Nprocs>-1
  ! NOTE: pass myid=-1 if the process ID is unknown.

  integer, intent(in) :: istat  ! error code (user defined), .gt.0 expected...

  !-----------------------
  integer :: jstat,id,mpierr
  !-----------------------

  jstat = max(1,istat)

  id=myid
  if(id.lt.0) then
#ifdef __MPI
     call MPI_COMM_RANK(mpi_env_comm,id,mpierr)
#else
     id=0
#endif
  endif

  write(0,*) ' ERRSET_MPI call by process myid=',id,'; istat = ',istat
  if(istat.ne.jstat) write(0,*) ' ...istat reset to: ',jstat

  mpi_errstat = jstat

  if(mpi_errstat_active) then
     call errchk_mpi(id)
     write(0,*) ' ??ERRSET_MPI: myid=',id,' unexpected return from ERRCHK_MPI'
  else
     write(0,*) ' ERRSET_MPI myid=',id,': immediate exit status code 1'
  endif
  call exit(1)

end subroutine errset_mpi

subroutine errchk_mpi(myid)

  ! check MPI error status, if collective error handling feature is enabled...

  use mpi_env_mod
  implicit NONE

  integer, intent(in) :: myid   ! process ID: 0,1,2,...<Nprocs>-1

  !---------------------
  integer :: istat_reduce, id, mpierr
  !---------------------
  ! do nothing, if feature is inactive...
  !---------------------

  if(.NOT.mpi_errstat_active) RETURN

#ifdef __MPI

  CALL MPI_ALLREDUCE(mpi_errstat,istat_reduce,1, &
       MPI_INTEGER,MPI_MAX, mpi_env_comm, mpierr)

#else

  istat_reduce = mpi_errstat
  mpierr = 0

#endif

  if(max(mpierr,istat_reduce).gt.0) then

     id = myid
     if(id.lt.0) then
#ifdef __MPI
        call MPI_COMM_RANK(mpi_env_comm,id,mpierr)
#else
        id=0
#endif
     endif

     write(0,*) ' ***ERRCHK_MPI on process myid=',id, &
          ' detects: istat_reduce=',istat_reduce,', mpierr=',mpierr

#ifdef __MPI
     CALL MPI_FINALIZE(mpierr)
#endif

     call exit(1)

  endif

end subroutine errchk_mpi
