c-----------------------------------------------
 
      subroutine cptimi(ztime)
      implicit none
 
      real ztime
c
c  DMC 5 Sep 1991
c
c    this routine is a more portable FORTRAN version of the old macro
c    CPTIMI.MAR.  It returns the amount of cpu time used so far by the
c    current process, in a floating point word, in units of HUNDREDTHS
c    of seconds *** NOTE FUNNY UNITS which are used for historical
c    reasons
c
c    usage:
c
c       CALL CPTIMI(ZTIME1)
c
c         execute code-to-be-timed
c
c       CALL CPTIMI(ZTIME2)
c
c       ZTIME = 0.01*(ZTIME2-ZTIME1)
c       TYPE 1001,ZTIME
c 1001	FORMAT('  CPU time used = ',1pe11.4,' seconds.')
c
c-------------------------------------------------
c
c  dmc May 2007 -- use standard f95 intrinsic routine "cpu_time"
c

      call cpu_time(ztime)
      ztime = 100.0*ztime   ! units returned: 100ths of a second...

      return
      end
