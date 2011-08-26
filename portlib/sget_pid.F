      subroutine sget_pid(ipid)
C
#ifdef __NAGWARE
      use f90_unix
#endif
C
      implicit none
C
      integer ipid                      ! process id number (output)
C
C  vms/unix portable routine -- return the process id number
C
#ifdef __VMS
      include '($JPIDEF)'
#endif
C
#ifdef __UNIX
#if __X1
      integer ierror
#elif !__NAGWARE
      integer getpid
#endif
#endif
C
#ifdef __VMS
      call lib$getjpi(JPI$_PID,,,ipid,,)
#endif
C

#ifdef __UNIX
#if __X1
      CALL PXFGETPID(ipid, ierror)
#else
      ipid = getpid()
#endif
#endif
C
      return
      end
C------------------------
      subroutine sget_pid_str(pidstr,ilen)
C
      implicit none
C
C  vms/unix portable routine -- return the process id number
C  as a character string
C
C  output:
      character*(*) pidstr      ! the PID, left justified, blank padded
      integer ilen              ! the non-blank length of pidstr returned.
C
C----------
C  local:
      integer ilenstr,ipid,icp,icz
      character*12 zbuf
C----------
C
      pidstr=' '
      ilenstr=len(pidstr)
C
      call sget_pid(ipid)
C
      write(zbuf,'(I12)') ipid
C
      icp=0
      do icz=1,12
         if(zbuf(icz:icz).ne.' ') then
            icp=icp+1
            if(icp.le.ilenstr) pidstr(icp:icp)=zbuf(icz:icz)
         endif
      enddo
      ilen=icp
C
      return
      end
