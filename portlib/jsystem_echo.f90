integer function jsystem_echo(cmd)

  implicit NONE
  character*(*), intent(in) :: cmd   ! input shell commend for "jsystem"

  ! issue a command to jsystem, but, capture its output with ">" to a
  ! temporary file.  Print the contents of the file, then, delete it.

  character*40 tmpfile
  character*20 pidstr
  character*120 lbuf
  integer :: ilenp,ilun,istat,istat_jsys

  integer :: jsystem

  !--------------------------------
  ! form temporary filename

  call sget_pid_str(pidstr,ilenp)
  tmpfile = '/tmp/'//pidstr(1:ilenp)//'_jsys.tmp'

  ! execute

  istat_jsys = jsystem(trim(cmd)//' > '//trim(tmpfile))

  ! try to open file 

  write(6,*) ' '
  write(6,*) ' %jsystem_echo: '//trim(cmd)
  write(6,*) ' %jsystem_echo: status: ',istat_jsys
  write(6,*) ' %jsystem_echo: stdout:'
  write(6,*) ' '

  call find_io_unit(ilun)

  open(unit=ilun,file=trim(tmpfile),status='old',iostat=istat)
  if(istat.eq.0) then

     ! echo file contents
     do
        read(ilun,'(A)',iostat=istat) lbuf
        if(istat.ne.0) exit
        write(6,'(1x,A)') trim(lbuf)
     enddo

     ! close and delete file
     close(unit=ilun,status='delete')
  endif

  ! jsystem status value is returned

  jsystem_echo = istat_jsys

end function jsystem_echo
