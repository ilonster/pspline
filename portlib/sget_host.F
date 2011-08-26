      subroutine sget_host(host)
      implicit none
      character*(*) host                ! computer host name, returned.
      integer ix                        ! loop variable
C
C  get the hostname, on various systems...
C
#ifdef __VMS
      include '($SYIDEF)'
#endif
#ifdef __UNIX
 
#if __CRAY || __X1
      character dummy(4)
      integer junam,ierr,ilen,stat
#elif __HP || __IBM || __RS6000
      integer gethostname
#else
      integer hostnm
#endif
 
      do ix=1,len(host)
         host(ix:ix)=' '  ! RGA, clear host in compiler safe manner for str_pad
      end do
#endif /* __UNIX  */
 
#ifdef __VMS
      call lib$getsyi(SYI$_NODENAME,,host,,,)
#endif
 
#ifdef __UNIX
 
#if __CRAY || __X1
      stat=0
      call pxfstructcreate('utsname',junam,ierr)
      stat=stat+abs(ierr)
      call pxfuname(junam,ierr)
      stat=stat+abs(ierr)
      ilen=0
      call pxfstrget(junam,'nodename',host,ilen,ierr)
      stat=stat+abs(ierr)
      call pxfstructfree(junam,ierr)
      if (stat/=0) host='UNKNOWN'
#elif __HP || __IBM || __RS6000
      if (gethostname(host) .ne. 0) host='UNKNOWN'
#else
      if (hostnm(host) .ne. 0) host='UNKNOWN'
#endif
 
#endif  /*  __UNIX  */
      call str_pad(host)
C
      return
      end
