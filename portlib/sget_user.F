      subroutine sget_user(zname)
      character*(*) zname
C
C  get the current username
C
#ifdef __VMS
      include '($JPIDEF)'    ! VMS only
C
      ITEM = JPI$_USERNAME
      ISTAT = LIB$GETJPI(ITEM,,,IRET,ZNAME,ILEN)
C
#endif
#ifdef __UNIX
      call ugetlog(zname)
#endif
      return
      end
