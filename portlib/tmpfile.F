C  tmpfile -- create a temporary filename, based on the current process id
C   mod dmc 5.11.98:  use pid instead of date-time
C
      subroutine tmpfile(zprefix,zfile,ilz)
c
      character*(*) zprefix  ! filename prefix (input)
      character*(*) zfile    ! temp filename (output)
      integer ilz            ! length of temp filename (output)
C
C  zprefix can just be the first part of a filename, or a
C  complete path spefication followed (optionally) by the first part
C  of a filename.
C----------------
C  local
C
      character*12 cpid
      integer str_length
C
C----------------
C
      zfile=' '
      ilzf=len(zfile)
C
      if(zprefix.eq.' ') then
         ilzp=0
      else
         ilzp=str_length(zprefix)
      endif
C
      call sget_pid_str(cpid,ilcp)
C
      if(ilzp.eq.0) then
         zfile = cpid(1:ilcp)//'.tmp'
      else
         zfile = zprefix(1:ilzp)//cpid(1:ilcp)//'.tmp'
      endif
C
      ilz=str_length(zfile)
C
      return
      end
C-----------------------------------------------------------------
      subroutine tmpfile_d(zprefix,zfile,ilz)
C
C  create a temporary file in a temporary files directory:
C
C  UNIX:  TMPDIR if defined, or else /tmp/
C  VMS:   JUNK   if defined, or else SYS$LOGIN
C
      character*(*) zprefix  ! filename prefix (input)
      character*(*) zfile    ! temp filename (output)
      integer ilz            ! length of temp filename (output)
C
C  local:
C
      character*200 zpath
C
      integer str_length
C--------------
C
      zfile=' '
C
#ifdef __UNIX
      call mpi_sget_env('TMPDIR',zpath,iertmp)
      ilpath=str_length(zpath)
      if(ilpath.eq.0) then
         zpath(1:5)='/tmp/'
         ilpath=5
      else
         if(zpath(ilpath:ilpath).ne.'/') then
            ilpath=ilpath+1
            zpath(ilpath:ilpath)='/'
         endif
      endif
#endif
C
#ifdef __VMS
      call sget_env('JUNK',zpath)
      ilpath=str_length(zpath)
      if(ilpath.eq.0) then
         call sget_env('SYS$LOGIN',zpath)
         ilpath=str_length(zpath)
      endif
#endif
C
      ilzp=str_length(zprefix)
      if(ilzp.gt.0) then
         zpath=zpath(1:ilpath)//zprefix(1:ilzp)
         ilpath=str_length(zpath)
      endif
C
      call tmpfile(zpath(1:ilpath),zfile,ilz)
C
      return
      end
