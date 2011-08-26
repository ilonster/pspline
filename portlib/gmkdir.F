#define BUFSIZ 200
!
      subroutine gmkdir(root_in,rpath_in,ier)
!
!  make a multi-level subdirectory starting from root
!
!  unix example:  call gmkdir('HOME','.cache/sub1/sub2/sub3',ier)
!    => $HOME/.cache/sub1/sub2/sub3 directory is created (if it does
!       not already exist).  Each higher level directory is also created
!       as needed.
!
!  vms example:  call gmkdir('SYS$LOGIN','[zcache.sub1.sub2.sub3]',ier)
!    => subdirectory chain [zcache.sub1.sub2.sub3] is created under the
!       user's login directory (SYS$LOGIN) -- each subdirectory level is
!       created as needed.
!
!  input:
      character*(*) root_in             ! root -- start point (must exist)
      character*(*) rpath_in            ! subdirectory chain
!
!  output:
      integer ier                       ! error code, 0 = OK
!
!-------------------------------------------------------------
!
      character*BUFSIZ start
      character*BUFSIZ next
!
      character*250 root,rpath
!
!-------------------------------------------------------------
!
      if(root_in.ne.' ') then
         root = root_in
         rpath = rpath_in
      else
         if(rpath_in(1:1).eq.'/') then
            root='/'
            rpath=rpath_in(2:)
         else if(rpath_in(1:1).eq.'~') then
            if(rpath_in(2:2).eq.'/') then
               root='HOME'
               rpath=rpath_in(3:)
            else
               ! (I don't think this can work: ~<username>)
               root=' '
               rpath = rpath_in
            endif
         else
            call sget_cwd(root)
            rpath = rpath_in    ! path relative to $cwd
         endif
      endif

      ier=0
      call gmkdir_init(root,start,rpath,next)
!
 10   continue
      if((ier.eq.0).and.(next.ne.' ')) then
         call gmkdir_next(start,next,ier)
         go to 10
      endif
!
      return
      end
!
!-------------------------------------------------------------
!
      subroutine gmkdir_init(root,start,rpath,next)
!
      character*(*) root                ! root (as passed)
      character*(*) start               ! starting directory (standardized)
!
      character*(*) rpath               ! relative path (as passed)
      character*(*) next                ! starting rel. path (standardized)
!-------------
      integer str_length
!
      character*BUFSIZ ztest
!---------------------------------
!
      start=' '
      next=' '
!
#ifdef __UNIX
!
!  root directory...
!
      if(root.ne.' ') call ufilnam(root,' ',start)
!
!  relative path from there...
!
      iclen=str_length(rpath)
      ic=0
!
 10   continue
      ic=ic+1
      if(ic.gt.iclen) go to 1000        ! reached the end
      if((ichar(rpath(ic:ic)).ne.0).and.(rpath(ic:ic).ne.' ').and.
     >   (rpath(ic:ic).ne.'~').and.(rpath(ic:ic).ne.'/')) then
         ic1=ic
      else
         go to 10
      endif
!
      ic=iclen+1
 20   continue
      ic=ic-1
      if(rpath(ic:ic).ne.'/') then
         ic2=ic
      else
         go to 20
      endif
!
      next=rpath(ic1:ic2)
      go to 1000
#endif
!-------------------
#ifdef __VMS
!
!  root directory...
!
      if(root.ne.' ') then
         itest=index(root,'[')
         if(itest.eq.0) itest=index(root,']')
         if(itest.eq.0) itest=index(root,'<')
         if(itest.eq.0) itest=index(root,'>')
         if(itest.ne.0) then
            start=root                  ! use "as is"
            go to 500
         endif
!
!  may be a logical name that has to be translated (e.g. SYS$LOGIN)
!
         iclen=str_length(root)
         ic=0
 110     continue
         ic=ic+1
         if((ichar(root(ic:ic)).ne.0).and.(root(ic:ic).ne.' ')) then
            ic1=ic
         else
            go to 110
         endif
!
         ic2=iclen
         if(root(ic2:ic2).eq.':') ic2=ic2-1
         call mpi_sget_env(root(ic1:ic2),ztest,iertmp)
         if(ztest.eq.' ') then
            start=root                  ! translation failed, use "as is"
         else
            ilz=str_length(ztest)
            if(ilz.gt.2) then
               if(ztest(ilz-1:ilz).eq.'.]') then
                  start=ztest(1:ilz-2)//']' ! rooted name, omit trailing .
               else if(ztest(ilz-1:ilz).eq.'.>') then
                  start=ztest(1:ilz-2)//'>' ! rooted name, omit trailing .
               else
                  start=ztest(1:ilz)    ! use translated value verbatim
               endif
            else
               start=ztest(1:ilz)       ! use translated value verbatim
            endif
         endif
      endif
!
!  relative path subdirectory...
!
 500  continue
      iclen=str_length(rpath)
      ic=0
!
 510  continue
      ic=ic+1
      if(ic.gt.iclen) go to 1000        ! reached the end
      if((ichar(rpath(ic:ic)).ne.0).and.(rpath(ic:ic).ne.' ').and.
     >   (rpath(ic:ic).ne.'[').and.(rpath(ic:ic).ne.'<').and.
     >   (rpath(ic:ic).ne.'.')) then
         ic1=ic
      else
         go to 510
      endif
!
      ic=iclen+1
 520  continue
      ic=ic-1
      if((rpath(ic:ic).ne.']').and.(rpath(ic:ic).ne.'>').and.
     >   (rpath(ic:ic).ne.'.')) then
         ic2=ic
      else
         go to 520
      endif
!
      next=rpath(ic1:ic2)
#endif
 1000 continue
      return
      end
!----------------------------------------------------------------------
!
      subroutine gmkdir_next(start,next,ier)
!
! check the next directory
!
      character*BUFSIZ start            ! current root
      character*BUFSIZ next             ! current relative path
!
      integer ier
!
!-----------------
!
      integer jsystem,str_length
!
      character*1 zdelim,zterm
      character*BUFSIZ dirfile,newdir
!
      character*512 zcmd
!-----------------
!
#ifdef __UNIX
      zdelim='/'
#else
      zdelim='.'
#endif
!
      ils=str_length(start)
      ilx=str_length(next)
!
      inext=index(next,zdelim)-1
      if(inext.le.0) then
         inext=ilx
      endif
!
      if(start.ne.' ') then
#ifdef __UNIX
         dirfile=start(1:ils)//next(1:inext)
#else
         dirfile=start(1:ils)//next(1:inext)//'.DIR'
         zterm=start(ils:ils)
         newdir=start(1:ils-1)//zdelim//next(1:inext)//zterm
#endif
      else
!
!  blank root
!
#ifdef __UNIX
         dirfile=next(1:inext)
#else
         dirfile=next(1:inext)//'.DIR'
         zterm=']'
         newdir='['//zdelim//next(1:inext)//zterm
#endif
      endif
      ild=str_length(dirfile)
#ifdef __UNIX
      newdir=dirfile(1:ild)//'/'
      ild=ild+1
#endif
      iln=str_length(newdir)
!-------------------------------------------
! make the shell command
!
#ifdef __UNIX
      zcmd='if test ! -d '//dirfile(1:ild)//'; then mkdir '//
     >   newdir(1:iln)//'; fi'
      istat = jsystem(zcmd)
      if(istat.ne.0) then
         ! try a 2nd time -- prevents failure due to "directory exists..."
         ! (a race condition can occur due to MPI threads on a common node)
         write(0,*) ' %gmkdir: retry: '//trim(zcmd)
         istat = jsystem(zcmd)
      endif
      if(istat.ne.0) then
         ! try a 3rd time -- prevents failure due to "directory exists..."
         ! (a race condition can occur due to MPI threads on a common node)
         write(0,*) ' %gmkdir: retry: '//trim(zcmd)
         istat = jsystem(zcmd)
      endif
#else
      zcmd='if f$search("'//dirfile(1:ild)//'").eqs."" then $ '//
     >   'create/directory '//newdir(1:ild)
      istat = jsystem(zcmd)
#endif
!
#ifdef __UNIX
      ier=istat
#else
      if(mod(istat,2).ne.1) then
         ier=istat
      else
         ier=0
      endif
#endif
!--------------------------------------------
!  new root and rel. path
!
      start=newdir
      if(inext.lt.ilx) then
         next=next(inext+2:ilx)
      else
         next=' '
      endif
!
      return
      end
 
