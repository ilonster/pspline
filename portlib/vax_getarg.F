C-----------------------------------------------------------------------
C  VAX_GETARG -- VAX versions of Unix routines IARGC and GETARG
C    IARGC_VMS -- return count of command line arguments
C    GETARG_VMS -- return nth command line argument (blank delimitted)
C
C  On a VAX, programs using these routines must be invoked as
C  "foreign commands" -- See the VAX/VMS users' guide.
C
#if !__CRAY && !__X1 
C$	program vgtest
C
C$	character*32 zarg
C
C$	inum=iargc_vms()
C$	do i=1,inum
C$	  call getarg_vms(i,zarg)
C$	  write(6,1001) i,zarg
C$ 1001	  format(1x,' arg no. ',i2,' is:  ',a)
C$	enddo
C
C$	stop
C$	end
#endif
C-----------------------------------------------------------------------
      subroutine vax_getarg
C
C  read the command line with LIB$GET_FOREIGN
C
#ifdef __UNIX
      write(6,'('' ?vax_getarg called from unix.'')')
      return
#endif
C
#ifdef __VMS
      COMMON/VAXGETB/ ARGS
      CHARACTER*256 ARGS
C
      DATA ICALL/0/
C
C---------------------------------------
C
      if(icall.eq.0) then
        icall=1
        ARGS=' '
        istatl=lib$get_foreign(ARGS,,igot)
        if(istatl.ne.1) then
          write(6,'('' ?vax_getarg:  lib$get_foreign status='',i10)')
     >        istatl
          ARGS=' '
        else
          il=len(ARGS)
          if(igot.lt.il) ARGS(igot+1:il)=' '
        endif
      endif
      return
C
      end
C-----------------------------------------------------------------------
C
      integer function iargc_vms()
C
C  count command line arguments
C
      COMMON/VAXGETB/ ARGS
      CHARACTER*256 ARGS
C
      call vax_getarg
C
      istart=1
      icount=0
 10   continue
        call vax_nexarg(istart,ifin)
        if(istart.lt.0) go to 90
        icount=icount+1
        istart=ifin+1
      go to 10
C
 90   continue
      iargc_vms=icount
      return
      end
C-----------------------------------------------------------------------
C
      subroutine getarg_vms(inum,argv)
C
      character*(*) argv
C
C  fetch the inum'th command line argument
C
      COMMON/VAXGETB/ ARGS
      CHARACTER*256 ARGS
C
      call vax_getarg
C
      argv=' '
C
      istart=1
      icount=0
 10   continue
        call vax_nexarg(istart,ifin)
        if(istart.lt.0) go to 90
        icount=icount+1
        if(icount.eq.inum) then
          argv=ARGS(istart:ifin)
          go to 90
        endif
        istart=ifin+1
      go to 10
C
 90   continue
      return
      end
C-----------------------------------------------------------------------
C
      subroutine vax_nexarg(istart,ifin)
C
C  find the next command line argument
C    istart = place to start search (input)
C
C    istart = -1 if nothing found (output)
C    ARGS(istart:ifin) = argument found (if any) (output)
C
C
      COMMON/VAXGETB/ ARGS
      CHARACTER*256 ARGS
C
      CHARACTER*1 ZBLANK,ZTAB
C
C-----------------------------------------------
C
      ztab=char(9)
      zblank=' '
C
      il=len(args)
C
      if(istart.gt.il) then
        istart=-1
        return
      endif
C
C  find non blank non tab character
C
      do ic=istart,il
        if((ARGS(ic:ic).ne.zblank).and.(ARGS(ic:ic).ne.ztab)) go to 10
      enddo
C
C  nothing found
C
      istart=-1
      return
C
C  something found
C
 10   continue
      istart=ic
C
C  find the end
C
      do ic=istart,il
        if((ARGS(ic:ic).eq.zblank).or.(ARGS(ic:ic).eq.ztab)) go to 20
      enddo
C
      ic=il+1
C
C  found the end
C
 20   continue
      ifin=ic-1
 
      return
#endif
      end
