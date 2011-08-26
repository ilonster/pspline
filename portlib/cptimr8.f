      subroutine cptimr8(ztime)
C
      IMPLICIT NONE
C
      REAL*8 ztime
      REAL zctime
C
C  this routine returns the elapsed CPU time.  Valid for UNIX or VMS
C  value returned is REAL*8.  Time in seconds.
C
      call cptimi(zctime)
      ztime=0.01*zctime   ! real -> real*8
C
      return
      end
