      subroutine gosleep(isecs)
C
#ifdef __NAGWARE
      use f90_unix_proc
#endif
C
      integer isecs                     ! number of seconds to sleep
C
C  this subroutine causes the program to go to sleep for `isecs' seconds!
C
#ifdef __VMS
      call lib$wait( float(max(0,isecs)) )
#endif
 
#ifdef __UNIX
      call csleep( max(0,isecs) )
#endif
      return
      end
