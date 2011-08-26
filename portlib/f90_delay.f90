subroutine f90_delay_r4(zdelay,zflops)

  !  REAL (usually real*4) interface to f90_delay which has a REAL*8
  !  interface.

  implicit NONE
  real, intent(in) :: zdelay   ! desired delay (seconds)
  real, intent(out) :: zflops  ! REAL*8 FLOPS executed to create this delay

  real*8 :: zdelay_r8,zflops_r8

  zdelay_r8 = zdelay
  call f90_delay(zdelay_r8,zflops_r8)
  zflops = zflops_r8

end subroutine f90_delay_r4

subroutine f90_delay(zdelay,zflops)

  implicit NONE
  real*8, intent(in) :: zdelay   ! desired delay (seconds)
  real*8, intent(out) :: zflops  ! REAL*8 FLOPS executed to create this delay

  !  Count by adding 1.0 to zflops repeatedly until a specified delay
  !  time (zdelay, seconds) has ellapsed.

  !  Use f90 intrinsic routine system_clock to get information on the 
  !  system clock; if there is no clock, get a delay (in seconds) using
  !  "gosleep" and set zflops = 0.0_R8

  integer :: i,j,icount0,icount,icount_rate,icount_max
  integer :: idelay,iincr,itarget,iwrap,jwrap,ierr

  !---------------------------------------

  call system_clock(icount0,icount_rate,icount_max)

  if(icount_max.gt.0) then
     iincr = 1 + zdelay*icount_rate
     if((icount_max/iincr).lt.10) then
        ierr=1
     else
        ierr=0
        itarget = icount0+iincr
        if(itarget.gt.icount_max) then
           iwrap = 1
           itarget = iincr - (icount_max-icount0) + 1
        else
           iwrap = 0
        endif
     endif
  else
     ierr=1
  endif

  if(ierr.eq.1) then
     zflops = -1.0d0
     idelay = 1 + zdelay
     call gosleep(idelay)
     return
  endif

  zflops = 0.0d0

  i=0
  idelay=10000

  do 

     i=i+1
     call system_clock(icount,icount_rate,icount_max)
     if(icount.lt.icount0) then
        jwrap=1
     else
        jwrap=0
     endif

     if((jwrap.ge.iwrap).and.(icount.ge.itarget)) exit

     do j=1,idelay
        zflops=zflops+1.0d0
     enddo

  enddo

end subroutine f90_delay

