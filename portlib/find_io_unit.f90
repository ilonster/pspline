SUBROUTINE find_io_unit(ilun)

  ! Return next available Fortran I/O Unit Specifier

  ! unit number returned in range MIN_ILUN (120) to MAX_ILUN (999)
  ! return ilun = -1 if none are available but it is probably not
  ! possible for ~1000 files to be open simultaneously for other system 
  ! reasons... (note: MIN_ILUN increased to 120 as pathscale compiler
  ! reserves units 100:102 and perhaps others will as well).

  IMPLICIT none
  INTEGER, INTENT(OUT) :: ilun
  LOGICAL :: open
  INTEGER, PARAMETER :: MIN_ILUN=120, MAX_ILUN=999

  integer :: iu,ifound

  ilun = -1
  ifound = 0

  DO iu=MIN_ILUN, MAX_ILUN
     INQUIRE(UNIT=iu, OPENED=open)
     IF (.NOT. open) THEN
        ifound=1
        EXIT
     ENDIF
  END DO

  if(ifound.eq.1) ilun = iu

END SUBROUTINE find_io_unit

SUBROUTINE find_io_unit_list(n,ilun_list)

  ! find N distinct available I/O unit numbers.
  ! 1 <= N <= 99 assumed!

  ! a file is opened on each unit found, so that the next query for an
  ! available unit number returns a different number

  implicit NONE

  integer, intent(in) :: n   ! number of I/O unit numbers desired, <= 99
  integer, intent(out) :: ilun_list(n)

  !---------------------
  ! local:

  integer, parameter :: imax=99
  integer :: ii,inum
  character*2 :: prefix
  character*100 :: filenames(imax)
  integer :: ilf(imax),ilen

  !---------------------

  if(n.le.0) return

  ilun_list = 0

  inum=min(imax,n)

  do ii=1,inum

     call find_io_unit(ilun_list(ii))

     if(ii.lt.inum) then

        write(prefix,'(I2.2)') ii
        call tmpfile('/tmp/find_io_unit_'//prefix//'_',filenames(ii),ilf(ii))
        ilen = ilf(ii)
        open(unit=ilun_list(ii),file=filenames(ii)(1:ilen),status='unknown')

     endif

  enddo

  do ii=inum-1,1,-1
     close(unit=ilun_list(ii),status='delete')
  enddo

end SUBROUTINE find_io_unit_list
