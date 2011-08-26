c------------------------------------------------------------/usr$trnlog
C
C  ADAPTED FROM SGLIB "usr$trnlog" ROUTINE -- DMC 6 SEP 1991
C   FOR "VAXONLY" LIBRARY IN CMSMMS SYSTEM
C
C  UNIX code is added -- substitute concept of environment
C  variable translation for concept of logical name translation
C  DMC Oct 1991
C
      subroutine UTRNLOG(table,loglnam,physnam,lphys,istat)
c
#ifdef __NAGWARE
      use f90_unix_env
#endif
c
c  translate logical name to physical name
c
c  arguments:
      character*(*) table	!table to search
      character*(*) loglnam	!logical name to be translated
      character*(*) physnam	!(return) physical name
      integer       lphys	!(return) length of physical name
      integer       istat	!(return) status code
C
C  ISTAT=1 DENOTES SUCCESS
C
C  on UNIX system:
C   table is ignored
C   loglnam is the environment variable name
C   physnam is the value returned
C   lphys is the length w/o trailing blanks of the value returned
C   istat is 1 unless lphys=0, in which case istat is 2 on exit.
C
C-----------------------------------------------------------------------
C  VAX ONLY CODE:
C
c  local variables:
      integer str_length
c
#ifdef __VMS
      include '($LNMDEF)'     ! VAX-ONLY CODE
c
      integer sys$trnlnm	!replaced sys$trnlog
      character*64 holdnam
      character*16 ttabl
      integer*4 lhold		!length of logical name
      integer*4 kattr		!string attributes
c
      structure /itmlst/
        integer*2 att_leng /4/	!length
        integer*2 att_code /lnm$_attributes/	!code
        integer*4 att_addr /0/
        integer*4 att_desc /0/
        integer*2 str_leng /64/	!length
        integer*2 str_code /lnm$_string/	!code
        integer*4 str_addr /0/
        integer*4 str_desc /0/
        integer*4 lnm_end  /0/
      endstructure
      record /itmlst/ trn
      logical more
c
c-----------------------------------------------------------------------
c
      ttabl = table			!set table name for translation
      if(ttabl.eq.' ') ttabl = 'LNM$FILE_DEV'
      ltabl = STR_LENGTH(ttabl)
      holdnam = loglnam		!copy logical name to local
      lhold = STR_LENGTH(holdnam)	!get length
c
      trn.att_addr=%loc(kattr)	!first item returns attributes
      trn.str_addr=%loc(physnam)	!second item returns name
      trn.str_desc=%loc(lphys)
      physnam=' '
      lphys=0
      istat=0
      more=.true.
c
      do while (more)
        igood = sys$trnlnm(lnm$m_case_blind,ttabl(1:ltabl),
     >		holdnam(1:lhold),,trn)
        if(istat.eq.0) istat=igood	!return status from first trans
c
        if(igood) then		!if success
          holdnam = physnam(1:lphys)	!set up to iterate
          lhold = lphys
          if((kattr.and.lnm$m_terminal).ne.0) more=.false.
        else
          more=.false.
        endif
      enddo
c	  	  	  	  check for process perm file
      if(ichar(physnam(1:1)) .eq. '1b'x) then
        physnam = physnam(5:lphys)	 !adjust length
        lphys = lphys - 4
      endif
c
      continue	    ! END VAX ONLY CODE
#endif
C-----------------------------------------------------------------------
C  UNIX CODE:
C
#ifdef __UNIX
      ist=1
      if(loglnam(1:1).eq.'$') ist=2
      call mpi_sget_env(loglnam(ist:),physnam,iertmp)
      lphys=STR_LENGTH(physnam)
      if(lphys.eq.0) then
        istat=2
      else
        istat=1
      endif
#endif
C
      return
C
      end
