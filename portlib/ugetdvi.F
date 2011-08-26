c------------------------------------------------------------/usr$getdvi
C
C  ADAPTED FROM SGLIB "usr$getdvi" ROUTINE -- DMC 6 SEP 1991
C   FOR "VAXONLY" LIBRARY IN CMSMMS SYSTEM
C
C  CALL TO THIS ROUTINE ON A UNIX SYSTEM IS FATAL!
C
C  THE GETDVI SYSTEM SERVICE IS VMS SPECIFIC!
C
      subroutine UGETDVI(physnam,klass,istat)
#ifdef __NAGWARE
      use f90_unix_proc
#endif
c
c  get device class for a physical device name
c
c  arguments:
      character*(*) physnam	!physical name of device
      integer       klass	!(return) device class
C			! 0=unknown, 1=disk, 2=terminal, 5=mailbox(nl:)
      integer       istat	!(return) status code
C
C-----------------------------------------------------------------------
C  BEGIN VAX ONLY CODE
c
c  local variables:
#ifdef __VMS
      include '($DVIDEF)'   ! VMS ONLY CODE STARTS WITH THIS LINE
      include '($DCDEF)'
c
      integer sys$getdviw
c
      structure /itmlst/
        integer*2 dvi_leng /4/	!length of response
        integer*2 dvi_code /dvi$_devclass/	!code for device class
        integer*4 dvi_bufr /0/	!address of response
        integer*4 dvi_resl /0/	!address of response length
      endstructure
c
      record /itmlst/ dvi
      integer class		!response buffer
      integer lresp		!response length
c
      integer lphys		!length of device name
C
c-----------------------------------------------------------------------
C
      lphys = len(physnam)
      lcol = index(physnam(1:lphys),' ')
      if(lcol.ne.0) lphys = lcol
c
      istat = 1		!assume success
      lcol = index(physnam(1:lphys),':')
      ldot = index(physnam(1:lphys),'.')
      if(ldot.ne.0 .and. lcol.eq.0) then
        klass = 1		!has a dot, but no colon => disk
        go to 800
      endif
      class = 0		!assume undefined
C
      dvi.dvi_leng = 4
      dvi.dvi_code = dvi$_devclass
      dvi.dvi_bufr = %loc(class)
      dvi.dvi_resl = 0	!%loc(lresp)
c
      igood = sys$getdviw(,,physnam(1:lphys),dvi,,,,)
cx	if(.not.igood) go to 800
c
      if(class.eq.0) then
        klass = 0	!unknown
      elseif(class.eq.dc$_term) then
        klass = 2	!terminal (on this node)
      elseif(class.eq.dc$_mailbox) then
        klass = 5
      else
        klass = 1
      endif
c
      istat = igood
 800  continue
      return
#endif
C-----------------------------------------------------------------------
#ifdef __UNIX
      WRITE(6,9991)
 9991 FORMAT(' ?UGETDVI:  NOT CALLABLE ON UNIX SYSTEM'/
     >         '  PROGRAMMING ERROR IN CALLER'/)
      CALL EXIT(1)
#endif
      end
