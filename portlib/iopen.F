C-----------------------------------------------------------------------
C  IOPEN -- VMS/UNIX collection of OPEN statements
C   this routine should not be called directly -- call via subroutine
C   GENOPEN -- the calling arguments are explained there.
C     argument checking is done in GENOPEN.
C
      integer function IOPEN(lun,fname,fstat,ftype,irecsz)
C
      integer lun          ! logical unit number
      character*(*) fname  ! file name
      character*(*) fstat  ! file status 'OLD' or 'NEW'
      character*(*) ftype  ! file type 'ASCII' 'BINARY' or 'DIRECT'
      integer irecsz       ! record size ('DIRECT' files only)
C
C  on VMS a condition handler is used to trap exceptions on OPEN
C  statement.  Purpose is to display system messages due to "unusual"
C  error conditions, and yet allow code execution to continue.
C
C  on UNIX the IOSTAT=IOS OPEN statement qualifier is used; errors
C  do not result in a message, just the setting of the IOSTAT code.
C
C----------------------------------------
C
#ifdef __VMS
      external IOPERH		! VMS error handler
C
      ISTAT=1			! VMS default
#endif
#ifdef __UNIX
      ISTAT=0				! UNIX default
#endif
C
C  establish VMS condition handler
C   NOTE -- on OPEN error on VMS, the condition handler takes over,
C    and forces an exit from this routine with the value of IOPEN set
C    to the "condition code" identifying the error.
C
C           on UNIX, the IOSTAT=ISTAT expression is used in the OPEN
C    statements to capture the error status of the OPEN operation, and
C    this is returned as the function value to the caller.
C
      inb=index(fname,' ')-1
      if(inb.le.0) inb=len(fname)
C
#ifdef __VMS
      call LIB$ESTABLISH(IOPERH)
#endif
C
      if(ftype.eq.'ASCII') then
C
        if(fstat.eq.'OLD') then
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status='OLD',READONLY)
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status='OLD',action='READ',
     >            IOSTAT=ISTAT)
#endif
C
        else
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',carriagecontrol='LIST')
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',IOSTAT=ISTAT)
#endif
C
        endif
C
      else if(ftype.eq.'BINARY') then
C
        if(fstat.eq.'OLD') then
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status='OLD',READONLY,
     >            access='SEQUENTIAL',form='UNFORMATTED')
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status='OLD',action='READ',
     >            access='SEQUENTIAL',form='UNFORMATTED',
     >            IOSTAT=ISTAT)
#endif
C
        else
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',form='UNFORMATTED')
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',form='UNFORMATTED',
     >            IOSTAT=ISTAT)
#endif
C
        endif
C
      else if(ftype.eq.'DIRECT') then
C
        if(fstat.eq.'OLD') then
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status='OLD',READONLY,
     >            access='DIRECT',recordsize=irecsz)
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status='OLD',action='READ',
     >            access='DIRECT',recl=irecsz,
     >            IOSTAT=ISTAT)
#endif
C
        else
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='DIRECT',recordsize=irecsz)
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='DIRECT',recl=irecsz,
     >            IOSTAT=ISTAT)
#endif
C
        endif
C
C
      else if(ftype.eq.'BLOCKD') then
C
        if(fstat.eq.'OLD') then
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status='OLD',READONLY,
     >            access='SEQUENTIAL',recl=irecsz,
     >            recordtype='FIXED',form='UNFORMATTED')
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status='OLD',action='READ',
     >            access='SEQUENTIAL',recl=irecsz,
     >            form='UNFORMATTED',IOSTAT=ISTAT)
#endif
C
        else
C
#ifdef __VMS
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',recl=irecsz,
     >            recordtype='FIXED',form='UNFORMATTED')
#endif
#ifdef __UNIX
          OPEN(unit=lun,file=fname(1:inb),status=fstat,
     >            access='SEQUENTIAL',recl=irecsz,
     >            form='UNFORMATTED',IOSTAT=ISTAT)
#endif
C
        endif
C
      endif
C
#ifdef __VMS
      call LIB$REVERT
#endif
C
      IOPEN=ISTAT
C
      return
      end
