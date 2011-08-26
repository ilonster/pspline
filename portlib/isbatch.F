      subroutine isbatch(iret)
C
C  DMC 9 SEP 1991
C
c
c  this fortran routine replaces the old macro "isbatch".  This routine
c  returns IRET=1 if the current process is a batch job or spawned
c  subprocess not receiving input from a terminal; it returns IRET=0
c  if an interactive terminal is in control.
c
c  the VAX code is lifted out of SGIOF.FOR (SGLIB) by M. Thompson
c  the subroutines are lifted out of the same source -- installed in
c  the TRANSP CMSMMS system with change of name and other changes to
c  support portability of code to unix and elsewhere off the VAXs.
c
c  the UNIX code just makes its determination by calling the system
c  routine "isatty".
c
 
C  VAX CODE:
#ifdef __VMS
      character*64 trmnam
 
        IRET = 0
 
        call utrnlog(' ','SYS$COMMAND',trmnam,lnam,igood)
        if(.not.igood) then	!trmnam.eq.' '
          type *, ' %ISBATCH:  Can''t translate SYS$COMMAND'
          go to 9000
        endif
c				check device type
        call ugetdvi(trmnam(1:lnam),klass,igood)
c
        if (klass.eq.0) then
 
          IRET = 0
          type *, ' %ISBATCH:  SYS$COMMAND device type unknown'
          type 1001, trmnam(1:lnam)
 1001     format(' %ISBATCH:  SYS$COMMAND was translated as:  ',A)
 
        else if(klass.eq.2) then
 
          IRET = 0
 
        else
 
          IRET = 1
 
        endif
#endif
C
C  END VAX CODE.
C
C  UNIX CODE:
C
#ifdef __UNIX
      INTEGER FISATTY
C
      IANS=FISATTY(0)
C
      IF(IANS.EQ.1) THEN
        IRET = 0
      ELSE
        IRET = 1
      ENDIF
C
#endif
C
 9000 continue
      return
      end
