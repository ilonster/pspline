subroutine mpi_share_env(myid,ierr)

  use mpi_env_mod
  implicit NONE

  ! share environment information btw processes in an MPI job
  !   (non MPI version: do almost nothing)

  integer, intent(in) :: myid   ! processor id number: 0 for "head node"
  integer, intent(out) :: ierr  ! status code returned: 0 for OK

#ifdef __MPI
  integer :: isizes(3)
  character, dimension(:), allocatable :: chbuf
#endif
  integer i,j,k,ilen,inumch,icwd

  !------------------------------------------------
  ! clear error code (this is the only action if no __MPI):

  ierr = 0

#ifdef __MPI

  if(myid.eq.0) then
     isizes(1)=n_alloc_names
     isizes(2)=n_active_names

     isizes(3)=0
     do i=1,n_active_names
        isizes(3) = isizes(3) + 1 + len(trim(mpi_env_names(i)))
        isizes(3) = isizes(3) + 1 + len(trim(mpi_env_vals(i)))
     enddo
  endif

  call MPI_BCAST(isizes, 3, MPI_INTEGER, 0, mpi_env_comm, ierr)
  if(ierr.ne.0) then
     call err('MPI_BCAST(isizes)')
     return
  endif

  inumch = isizes(3)

  allocate(chbuf(inumch))

  if(myid.ne.0) then
     n_alloc_names=isizes(1)
     n_active_names=isizes(2)

     if(allocated(mpi_env_names)) deallocate(mpi_env_names,mpi_env_vals)
     allocate(mpi_env_names(n_alloc_names),mpi_env_vals(n_alloc_names))

     do i=1,n_alloc_names
        mpi_env_names(i)=' '
        mpi_env_vals(i)=' '
     enddo
  endif

  if(myid.eq.0) then
     j=0
     do i=1,n_active_names
        call trans(mpi_env_names(i))
        call trans(mpi_env_vals(i))
     enddo
  endif

  call MPI_BCAST(chbuf, inumch, MPI_CHARACTER, 0, mpi_env_comm, ierr)
  if(ierr.ne.0) then
     call err('MPI_BCAST(chbuf)')
     return
  endif

  if(myid.gt.0) then
     j=0
     do i=1,n_active_names
        call rcv(mpi_env_names(i))
        call rcv(mpi_env_vals(i))
     enddo
  endif

#endif

  ! serial version also...

  icwd=0
  do i=1,n_active_names
     if(mpi_env_names(i).eq.'_cwd') icwd=i
  enddo

  if(icwd.gt.0) then
     call gmkdir(' ',mpi_env_vals(icwd),ierr)
     if(ierr.ne.0) then
        call err("gmkdir(cwd...)")
        return
     endif
  endif

  if(icwd.gt.0) then
     ! go to directory specified as "cwd"
     ! make sure it exists

     call sset_cwd(mpi_env_vals(icwd),ierr)
     if(ierr.ne.0) then
        call err('sset_cwd(cwd='//trim(mpi_env_vals(icwd))//')')
        return
     else
        write(0,*) ' (mpi_share_env) process myid=',myid,' cwd: ', &
             trim(mpi_env_vals(icwd))
     endif

  endif

CONTAINS

#ifdef __MPI

  subroutine trans(str)
    character*(*), intent(in) :: str

    ! load non-blank part of str into buffer
    ! side effect: j is modified... chbuf becomes concatenated set of
    !   non-blank strings terminated by nulls

    ilen=len(trim(str))
    do k=1,ilen
       j=j+1
       chbuf(j) = str(k:k)
    enddo
    j=j+1
    chbuf(j)=char(0)
  end subroutine trans

  subroutine rcv(str)
    character*(*), intent(inout) :: str

    ! fill in non-blank parts of str from buffer; done when null is found
    ! side effect: j is modified...

    integer :: icv

    k=0
    do 
       j=j+1
       icv = ichar(chbuf(j))
       if(icv.ne.0) then
          k=k+1
          str(k:k) = chbuf(j)
       else
          exit
       endif
    enddo

  end subroutine rcv

#endif

  subroutine err(errstr)

    character*(*), intent(in) :: errstr

    write(0,*) ' ?? mpi_share_env: error on process myid = ',myid
    write(0,*) '    message: '//trim(errstr)

  end subroutine err

end subroutine mpi_share_env
