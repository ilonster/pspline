module mpi_env_mod

  ! module to contain environment information, to be shared across MPI
  ! processes perhaps.

  IMPLICIT NONE

#ifdef __MPI
  include 'mpif.h'
#endif

  PUBLIC
  SAVE

  ! this module is local or "private" to the portlib library.  It is only
  ! meant to be accessed by select portlib fortran routines.

  ! this module implements an MPI shared "environment".  The head node is
  ! to establish $cwd, command line arguments, and "environment variables"
  ! all stored in the memory of this module.  Then, MPI_BCAST is used to
  ! distribute this information to the child nodes.

  ! see the portlib library routines that use this module:

  ! mpi_env_comm_set(icomm) ! set MPI communicator: default is MPI_COMM_WORLD
  ! mpi_env_comm_get(icomm) ! return MPI communicator.

  ! mpi_sget_env(name,value,ierr)
  !   if name is in module, return corresponding value
  !   if not, get from environment using sget_env; store result in module.
  !   ierr-- reteurn code; 0 for normal

  ! mpi_sset_env(name,value,ierr)
  !   put name and value in module; if name already there, replace value.

  ! mpi_share_env(myid,ierr)
  !   head node (myid.eq.0): broadcast module contents;
  !   compute nodes (myid.ne.0): receive module contents.
  !   if no MPI: this routine does nothing.

  ! mpi_printenv(ilun)
  !   print environment on unit (ilun)

  ! automatic environment: on first call, the current working directory
  ! is stored under the name "_cwd", command line argument count as "_nargs",
  ! and arguments as "_argv_1", "_argv_2", etc.  These values can be 
  ! overwritten by subsequent mpi_sset_env calls.

  ! NOTE: arguments from MPICH, I attempt to screen out...

  !------------------------
  ! *** distributed error handling ***
  ! NOTE: applications must be coded to use this properly, so, this
  !       feature must be explicitly enabled for use

  ! call errset_mpi_enable ... to activate this feature (call on all procs)
  !      (it is inactive by default)

  ! call errset_mpi_disable ... to deactivate this feature (call on all procs)

  ! ==> panic on process #myid ??
  ! call errset_mpi(myid,istat) ! SET a status code, istat > 0 expected
  !   this routine does NOT return !!!
  !   the routine does an MPI_ALLREDUCE on the module variable "mpi_errstat"
  !   then does MPI_FINALIZE then does call exit(1) !!!

  ! ==> check for error before MPI collective operation?
  ! call errchk_mpi(myid)
  !   this routine does an MPI_ALLREDUCE on the module variable "mpi_errstat"
  !   if it comes back non-zero then:  MPI_FINALIZE, call exit(1) !!!
  !   NO RETURN, if an error status is detected.

  ! the idea is, call this before each MPI global operation
  !   (MPI_REDUCE, MPI_ALLREDUCE, MPI_BCAST, MPI_BARRIER)

  ! portlib routine "bad_exit" modified to use this facility

  !------------------------
  ! ierr return codes:

  integer, parameter :: mpi_env_normal = 0

  integer, parameter :: mpi_env_name_error = 1
  ! name error, name rules: alphanumeric + "_" only; names are case sensitive
  !                         no imbedded blanks or control characters
  !                         non-blank length <= maxlen_name

  integer, parameter :: mpi_env_val_error = 2
  ! value error, value rules: no null or control characters.
  !                         non-blank length <= maxlen_val

  !------------------------
  ! maxlen parameters

  integer, parameter :: maxlen_name = 32
  integer, parameter :: maxlen_val = 180

  !------------------------
  ! variables 

#ifdef __MPI
  integer :: mpi_env_comm = MPI_COMM_WORLD
#else
  integer :: mpi_env_comm = -1
#endif

  integer :: n_alloc_names = 0
  integer :: n_active_names = 0

  character*(maxlen_name), dimension(:), allocatable :: mpi_env_names
  character*(maxlen_val),  dimension(:), allocatable :: mpi_env_vals

  !------------------------
  ! shared error status code:

  integer :: mpi_errstat = 0
  logical :: mpi_errstat_active = .FALSE.

  !------------------------

CONTAINS

  subroutine legal_name(namstr,ilen,ierr)
    character*(*), intent(in) :: namstr
    integer, intent(out) :: ilen
    integer, intent(out) :: ierr

    ! determine if name is legal; return non-blank length

    !------------------------
    character*63, parameter :: lchars = &
         '0123456789abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    integer :: ic,iloc
    !------------------------

    ilen = len(trim(namstr))

    if((ilen.eq.0).or.(ilen.gt.maxlen_name)) then
       ! illegal length
       ierr = mpi_env_name_error
       return

    else
       ierr = mpi_env_normal
    endif

    iloc = index(lchars,namstr(1:1))
    if(iloc.le.10) then
       ! digit or illegal character in 1st character position
       ierr = mpi_env_name_error
       return
    endif

    do ic=2,ilen
       iloc = index(lchars,namstr(ic:ic))
       if(iloc.le.0) then

          ierr = mpi_env_name_error
          exit
       endif
    enddo

  end subroutine legal_name

  subroutine legal_val(valstr,ilen,ierr)
    character*(*), intent(in) :: valstr
    integer, intent(out) :: ilen
    integer, intent(out) :: ierr

    ! determine if value is legal; return non-blank length

    !------------------------
    integer :: ic,ichv
    !------------------------

    ilen = len(trim(valstr))
    ierr = mpi_env_normal

    do ic=1,ilen
       ichv = ichar(valstr(ic:ic))
       ! if ASCII code is 127 or < 32, it is an unprintable character,
       ! deemed illegal for value strings
       if((ichv.lt.32).or.(ichv.ge.127)) then
          ierr=mpi_env_val_error
          exit
       endif
    enddo

  end subroutine legal_val

  subroutine lookup_alloc(name_in,iadr,inew)

    character*(*), intent(in) :: name_in
    integer, intent(out) :: iadr
    logical, intent(out) :: inew

    ! find slot for name (which should already have been checked for legality).

    ! allocate space if necessary
    ! if first call, create _cwd name-value pair

    !-------------------------
    integer :: ia,ifound,ialloc,nargs,ibrk
    character*(maxlen_name), dimension(:), allocatable :: tmp_names
    character*(maxlen_val),  dimension(:), allocatable :: tmp_vals
    character*(maxlen_val) :: argval
    !-------------------------

    if(n_alloc_names.eq.0) then

       ! first call

       call get_arg_count(nargs)
       nargs=max(0,min(99,nargs))

       !  screen MPICH process args...
       !  these seem to come after user args...

       ibrk=0
       do ia=1,nargs
          call get_arg(ia,argval)
          if(argval.eq.'-p4pg') then
             ibrk=ia
             exit
          endif
       enddo

       if(ibrk.gt.0) nargs=ibrk-1

       n_alloc_names=4
       do
          if(n_alloc_names.ge.(nargs+3)) exit
          n_alloc_names = n_alloc_names*2
       enddo

       allocate(mpi_env_names(n_alloc_names),mpi_env_vals(n_alloc_names))

       n_active_names=2
       mpi_env_names(1)='_cwd'
       mpi_env_names(2:)=' '
       mpi_env_vals(:)=' '

       call sget_cwd(mpi_env_vals(1))

       mpi_env_names(2)='_nargs'
       if(nargs.le.9) then
          write(mpi_env_vals(2),'(I1)') nargs
       else if(nargs.le.99) then
          write(mpi_env_vals(2),'(I2)') nargs
       endif

       do ia=1,nargs
          if(ia.le.9) then
             write(mpi_env_names(ia+2),'(a,i1)') "_argv_",ia
          else if(ia.le.99) then
             write(mpi_env_names(ia+2),'(a,i2)') "_argv_",ia
          endif

          call get_arg(ia,mpi_env_vals(ia+2))
          n_active_names = n_active_names + 1
       enddo
             
    endif

    ifound = 0
    do ia=1,n_active_names
       if(name_in.eq.mpi_env_names(ia)) then
          ifound=ia
          exit
       endif
    enddo

    if(ifound.gt.0) then
       iadr = ifound
       inew = .FALSE.
       return
    endif

    ! new name
    n_active_names = n_active_names + 1

    if(n_active_names.gt.n_alloc_names) then
       ! need to provide space
       ! copy data to temp arrays; reallocate with more space & copy back

       ialloc = n_alloc_names
       n_alloc_names = n_alloc_names*2

       allocate(tmp_names(ialloc),tmp_vals(ialloc))

       do ia=1,ialloc
          tmp_names(ia) = mpi_env_names(ia)
          tmp_vals(ia) = mpi_env_vals(ia)
       enddo
       
       deallocate(mpi_env_names,mpi_env_vals)
       allocate(mpi_env_names(n_alloc_names),mpi_env_vals(n_alloc_names))

       do ia=1,ialloc
          mpi_env_names(ia) = tmp_names(ia)
          mpi_env_vals(ia) = tmp_vals(ia)
       enddo
       do ia=ialloc+1,n_alloc_names
          mpi_env_names(ia) = ' '
          mpi_env_vals(ia) = ' '
       enddo

    endif

    inew = .TRUE.
    iadr = n_active_names
    mpi_env_names(iadr) = name_in
       
    ! NOTE: value not yet set.

  end subroutine lookup_alloc
end module mpi_env_mod
