      subroutine sget_dsk(dsk)
      character*(*) dsk
C
C  get current working disk (intended for VMS)
C
      character*32 zdir
C
      call showdefl(dsk,ildisk,zdir,ildir)
C
      return
      end
 
