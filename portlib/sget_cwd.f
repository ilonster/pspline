      subroutine sget_cwd(cwd)
      character*(*) cwd
C
C  get current working directory (VMS or UNIX)
C
      character*32 zdisk
      integer str_length,ilen,ilenc
C
      call showdefl(zdisk,ildisk,cwd,ilcwd)
      if(zdisk.ne.' ') then
         ilen=str_length(zdisk)
         if(zdisk(ilen:ilen).ne.':') then
            ilen=ilen+1
            zdisk(ilen:ilen)=':'
         endif
         ilenc=str_length(cwd)
         do i=min(len(cwd),ilenc+ilen),ilen+1,-1
            cwd(i:i)=cwd(i-ilen:i-ilen)
         enddo
         cwd(1:ilen)=zdisk(1:ilen)
      endif
C
      return
      end
