      SUBROUTINE SHOWDEFL(DEVNAM,LDEV,DIRNAM,LDIR)
C
#ifdef __NAGWARE
      use f90_unix_dir
#endif
      implicit none
C
C  GET THE CURRENT DEFAULT DISK AND DIRECTORY BY PARSING A DUMMY
C  FILENAME WITH THE RMS SERVICE CALL
C  DMC:  FOLLOWING HINTS BY M. THOMPSON
C
 
      CHARACTER*(*) DEVNAM  ! OUTPUT DEVICE NAME
      CHARACTER*(*) DIRNAM  ! OUTPUT DIRECTORY NAME
 
      INTEGER LDEV	      ! OUTPUT LENGTH OF DEVICE NAME
      INTEGER LDIR	      ! OUTPUT LENGTH OF DIRECTORY NAME
 
#ifdef __UNIX
      integer str_length
      integer istat
#ifndef __NAGWARE
      integer getcwd
#endif
 
#endif  /* __UNIX */
 
C-----------------------------------------------------------------------
C  VAX CODE
#ifdef __VMS
      CHARACTER*8 DUMNAM/'FOOO.FOO'/	! DUMMY FILENAME
C
      devnam=' '
      dirnam=' '
      CALL SHOWFILE(DUMNAM,DEVNAM,LDEV,DIRNAM,LDIR)
#endif
C
C-----------------------------------------------------------------------
C  UNIX CODE -- DMC 22 May 1992
C
#ifdef __UNIX
      ldev=0
      devnam=' '
C
      ldir=0
      dirnam=' '
#ifdef __NAGWARE
      call getcwd(dirnam,ldir,istat)
#elif __X1
      call cgetcwd(dirnam,ldir,istat)
#else
      istat=getcwd(dirnam)
#endif
C
#if __CRAY
      if (istat.le.0) dirnam='UNKNOWN'
#elif __HP
      if (istat.eq.0) then
         print *,' ...sget_cwd:showdefl - dirnam =',dirnam
         if (dirnam(1:1) .eq. ' ') dirnam='UNKNOWN'
      endif
#elif __IBM || __RS6000 || __SGI
      if (istat.eq.0) dirnam='UNKNOWN'
#endif
C
      call str_pad(dirnam)
C
      ldir=str_length(dirnam)
#endif  /* __UNIX */
C
      RETURN
      END
