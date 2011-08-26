      program porttest
C
#include "fpreproc/byte_declare.h"
C
C  dmc 9 Apr 1999
C
C  do a hodge podge test of portlib routines -- routines written for
C  portability btw OS / environments
C
      double precision dans,d1mach
C
      real rans
C
      character*10 hname
C
      character*100 value,hvalue,save
      character*5 ttest
C
      character*9 z9date
      character*24 z24date
C
      character*150 tmp_filename,tmp_file2
C
      character*50 ctest1,ctest2
C
      integer str_length
C
      integer fisatty
C
      integer :: ilun_list(3)
C
      real data1(50),data2(50)
C
 
      byte_declare BUFFER(512,2)
      byte_declare BUFF2(512,2)
C
#ifdef __VMS
      data hname/'SYS$LOGIN'/
#endif
#ifdef __UNIX
      data hname/'HOME'/
#endif
C
C-----------------------------------------------
C
      call err_ini
C
      call cptimer(ztime1)
      call wall_seconds(iwtime1)
C
      call get_arg_count(numargs)
      write(6,1000) numargs
 1000 format(
     >   ' (call get_arg_count(numargs)):',
     >   '  porttest command line: ',i2,' arguments.')
      if(numargs.gt.0) then
         write(6,*) '(call get_arg(i,arg)):  arguments follow:'
      endif
      do i=1,numargs
         call get_arg(i,value)
         ilv=str_length(value)
         write(6,'(6x,i2,'':  '',a)') i,value(1:ilv)
      enddo
C
      write(6,*) ' '
C
C  test fetching proc. id, & translating env. variable...
      call sget_pid(ipid)
C
      ilhn = str_length(hname)
      write(6,1001) ipid,hname(1:ilhn)
 1001 format(' ...starting PORTtest...'//
     >' call sget_pid(ipid) ... ipid=',i12//
     >' call sget_env(''',a,''',hvalue) ...'/)
C
      call mpi_sget_env(hname,hvalue,ierr)
      ilv=max(1,str_length(hvalue))
C
C  test call, no effect:
      call mpi_sset_env(hname,hvalue,ierr)
C
      call mpi_printenv(6,0)
C
      write(6,1010) hvalue(1:ilv)
 1010 format(' user login directory:  "',a,'"'/)
      save=hvalue
C
      call sget_user(value)
      ilv=str_length(value)
      write(6,1012) value(1:ilv)
 1012 format(' (call sget_user(username)) => username = ',a/)
C
      call sget_host(value)
      ilv=str_length(value)
      write(6,1013) value(1:ilv)
 1013 format(' (call sget_host(hostname)) => hostname = ',a/)
C
      call sget_cwd(value)
      ilv=str_length(value)
      write(6,1015) value(1:ilv)
 1015 format(/' (call sget_cwd(dirname)) current working directory:'/
     >   6x,a/)
C
#ifdef __VMS
      call sget_dsk(value)
      ilv=str_length(value)
      write(6,1016) value(1:ilv)
 1016 format(/' (call sget_dsk(disk)) current working disk:',2x,a/)
#endif
C
#ifdef __UNIX
      call ufilnam('HOME','foo.test',tmp_file2)
      ilt=str_length(tmp_file2)
      write(6,1018) tmp_file2(1:ilt)
 1018 format(/' call ufilnam(''HOME'',''foo.test'',xyz) ==>'/
     >        '      xyz = "',a,'"'/)
C
      write(6,'('' ...changing current working directory.'')')
      ils=str_length(save)
      call sset_cwd(save(1:ils),ierr)
      if(ierr.ne.0) write(6,*) ' *** error changing directory!'
C
      call sget_cwd(value)
      ilv=str_length(value)
      write(6,1015) value(1:ilv)
#endif
C-------------------------------
C  test date & date/time fetching...
C
      call c9date(z9date)
      call c24date(z24date)
      write(6,1020) z9date,z24date
C
 1020 format(' ...current date and time...'/
     >   ' call c9date(str)  => str="',a,'"'/
     >   ' call c24date(str) => str="',a,'"'/)
C
C------------------------------
C  test c <-> fortran string mapping
      ctest1='this is a test'
      call cstring(ctest1,buffer,'2C')  ! ctest1 => byte array "buffer"
      call cstring(ctest2,buffer,'2F')  ! byte array "buffer" => ctest2
      if(ctest1.eq.ctest2) then
         write(6,*) ' cstring test -- OK'
      else
         write(6,*) ' cstring test -- ERROR: ctest1.ne.ctest2'
         write(6,'(''   ctest1 = "'',a,''"'')') ctest1
         write(6,'(''   ctest2 = "'',a,''"'')') ctest2
      endif
      write(6,*) ' '
C------------------------------
C  test single character input (no echo) from stdin
C  test character stream output to stdout
C
      itty=fisatty(0)
      if(itty.eq.0) then
         write(6,*) ' (fisatty)...stdin is a FILE'
      else if(itty.eq.1) then
         write(6,*) ' (fisatty)...stdin is a TTY'
      else
         write(6,*) ' *** fisatty returned unexpected value: ',itty
      endif
C
      call term_str_out(' *** input 5 characters (no echo)-->')
      do ii=1,5
         call term_char_in(ttest(ii:ii))
      enddo
C
      call term_str_out(' ...echo:  '//ttest)
C
      write(6,'('' <all on one line>''/)')
C
C------------------
C  test fetching of a list of distinct & available fortran I/O unit numbers
C
      call find_io_unit_list(3,ilun_list)
      write(6,'(" 3 available fortran I/O unit numbers: ",3(1x,i4))')
     >     ilun_list(1:3)
C
C------------------
C  test 512-block byte-stream i/o (can be used for simple portable files).
C
      call tempfile('cft_test',tmp_filename,iltmp)
C
      icft_lun=99
C
      do i=1,512
#if __CRAY || __SX
         buffer(i,1)=char(int(50.0*(1.0+sin(float(i)))))
         buffer(i,2)=char(int(50.0*(1.0+cos(float(i)))))
#else
         buffer(i,1)=50.0*(1.0+sin(float(i)))
         buffer(i,2)=50.0*(1.0+cos(float(i)))
#endif
      enddo
C
      write(6,1030) tmp_filename(1:iltmp)
 1030 format(/' testing 512-block byte oriented i/o, filename:'/6x,a)
C
      call cftopen(icft_lun,tmp_filename(1:iltmp),'wb',0,ier)
      if(ier.ne.0) then
         write(6,*) '*** cftopen (write) error, ier=',ier
         go to 50
      endif
C
      call cftwrite(icft_lun,buffer(1,1),ier)
      if(ier.ne.0) then
         write(6,*) '*** cftwrite error (1), ier=',ier
         go to 15
      endif
      call cftwrite(icft_lun,buffer(1,2),ier)
      if(ier.ne.0) then
         write(6,*) '*** cftwrite error (2), ier=',ier
         go to 15
      endif
      write(6,*) '...cft write OK...'
C
 15   continue
      call cftclose(icft_lun)
      if(ier.ne.0) go to 50
C
C---
C
      call cftopen(icft_lun,tmp_filename(1:iltmp),'rb',0,ier)
      if(ier.ne.0) then
         write(6,*) '*** cftopen (read) error, ier=',ier
         go to 50
      endif
C
      ipass=0
 20   continue
      ipass=ipass+1
      call cftread(icft_lun,buff2(1,1),ier)
      if(ier.ne.0) then
         write(6,*) '*** cftread error (1), ier=',ier
         go to 25
      endif
      call cftread(icft_lun,buff2(1,2),ier)
      if(ier.ne.0) then
         write(6,*) '*** cftread error (2), ier=',ier
         go to 25
      endif
C
      ierdif=0
      do i=1,512
         if(buffer(i,1).ne.buff2(i,1)) then
            ierdif=ierdif+1
         endif
         if(buffer(i,2).ne.buff2(i,2)) then
            ierdif=ierdif+1
         endif
      enddo
C
      if(ierdif.gt.0) then
         write(6,*) '*** cft readback buffer .ne. write buffer!'
         go to 25
      else
         write(6,*) '...cft readback successful...'
      endif
C
      if(ipass.eq.1) then
         call cftrewnd(icft_lun,ier)
         if(ier.ne.0) then
            write(6,*) '*** cftrewnd error, ier=',ier
            go to 25
         else
            write(6,*) '...cft rewind OK...'
            go to 20
         endif
      endif
C
 25   continue
      call cftclose(icft_lun)
      write(6,'(''   cft i/o test completed.''/)')
C
 50   continue
C
      write(6,*) '...showing directory listing of file...'
#ifdef __UNIX
      istat=jsystem('ls -l '//tmp_filename(1:iltmp))
#endif
#ifdef __VMS
      istat=jsystem('dir/size/date '//tmp_filename(1:iltmp))
#endif
      write(6,*) '...jsystem (ls/dir) return status: ',istat
C
      call tempfile('port_test',tmp_file2,iltmp2)
      write(6,*) '...file rename test.'
      call frename(tmp_filename(1:iltmp),tmp_file2(1:iltmp2),istat)
      write(6,*) '...frename return status: ',istat
C
      write(6,*) '...showing directory listing of file...'
#ifdef __UNIX
      istat=jsystem('ls -l '//tmp_file2(1:iltmp2))
#endif
#ifdef __VMS
      istat=jsystem('dir/size/date '//tmp_file2(1:iltmp2))
#endif
      write(6,*) '...jsystem (ls/dir) return status: ',istat
C
      write(6,*) '...deleting file'
      call fdelete(tmp_file2(1:iltmp2),istat)
      write(6,*) '...fdelete return status: ',istat
      write(6,*) ' '
C
      write(6,*) '...showing directory listing of file...'
#ifdef __UNIX
      istat=jsystem('ls -l '//tmp_file2(1:iltmp2))
#endif
#ifdef __VMS
      istat=jsystem('dir/size/date '//tmp_file2(1:iltmp2))
#endif
      write(6,*) '...jsystem (ls/dir) return status: ',istat
      write(6,*) ' '
C
      ierno=ierfnof(88)
      open(unit=88,file=tmp_file2(1:iltmp2),status='old',iostat=ierno2)
      if(ierno.eq.ierno2) then
         write(6,*) '...ierfnof(lun)=',ierno,' matches iostat',
     >      ' from open of non-existent file'
      else
         write(6,*) '...ierfnof(lun)=',ierno,' DOES NOT MATCH iostat',
     >      ' from open of non-existent file'
      endif
C
C  test ascii max record length routine
C
      write(6,*) ' '
      open(unit=88,file=tmp_file2(1:iltmp2),status='unknown'
#ifdef __VMS
     >   ,carriagecontrol='LIST'
#endif
     >   )
      write(88,'(A)') 'this is a test'
      write(88,'(A)') 'this is also a test'
      close(unit=88)
c
      write(6,*) ' testing: portlib/max_reclen.for '
      ians=max_reclen(tmp_file2(1:iltmp2))
      write(6,*) ' max record length test result (should be 19):  ',ians
c
      open(unit=88,file=tmp_file2(1:iltmp2),status='old'
#ifdef __VMS
     >   ,carriagecontrol='LIST'
#endif
     >   )
      close(unit=88,status='delete')
C
C------------------
C  test direct access i/o
C
      do i=1,50
         data1(i)=cos(float(i))
      enddo
C
      irecsz=10
      call genopen(88,tmp_file2(1:iltmp2),'NEW','DIRECT',irecsz,ios)
      if(ios.ne.0) then
         write(6,'('' *** genopen (NEW) error *** ios = '',i5)') ios
         close(unit=88,err=100)
         go to 100
      endif
C
      do i=1,5
         ie1=1+10*(i-1)
         ie2=ie1+9
         write(88,rec=i) (data1(ie),ie=ie1,ie2)
      enddo
C
      close(unit=88)
C
      call is_ascii(tmp_file2(1:iltmp2),iascii,ier)
      if(iascii.eq.1) then
         write(6,*) ' ?? porttest:  is_ascii said binary file is ascii!'
      endif
      if(ier.ne.0) then
         write(6,*) ' ?? porttest:  is_ascii returned error code:  ',ier
      endif
C
      call genopen(88,tmp_file2(1:iltmp2),'OLD','DIRECT',irecsz,ios)
      if(ios.ne.0) then
         write(6,'('' *** genopen (OLD) error *** ios = '',i5)') ios
         close(unit=88,err=100)
         go to 100
      endif
C
      i=1
      do j=1,5
         i=i+2
         if(i.gt.5) i=i-5
         ie1=1+10*(i-1)
         ie2=ie1+9
         read(88,rec=i) (data2(ie),ie=ie1,ie2)
      enddo
C
      close(unit=88)
C
      idif=0
      do i=1,50
         if(data1(i).ne.data2(i)) idif=idif+1
      enddo
C
      if(idif.gt.0) then
         write(6,'('' *** direct access data changed on readback!!'')')
         write(6,'(''     recordsize was 10 words:'')')
         do i=1,50
            write(6,'(3x,i2,2(1pe12.5))') i,data1(i),data2(i)
         enddo
      else
         write(6,1090)
 1090    format(/' PortTest:  fortran direct access i/o test OK.'/)
      endif
C
 100  continue
      call fdelete(tmp_file2(1:iltmp2),istat)
      write(6,*) '...fdelete return status: ',istat
      write(6,*) ' '
C
C------------------
      write(6,*) '...call cvtdays(nseconds) tests:'
      write(6,*) ' '
      call cvtd_test(10000)
      call cvtd_test(500000)
C------------------
      write(6,*) ' '
      write(6,*) 'i1mach test...'
      do i = 1,16
         ians=i1mach(i)
         write(6,'('' i1mach('',i2,'')='',i20)') i,ians
      enddo
      write(6,*) ' '
      write(6,*) 'r1mach test...'
      do i = 1,5
         rans=r1mach(i)
         write(6,'('' r1mach('',i1,'')='',1pe14.7)') i,rans
      enddo
      write(6,*) ' '
      write(6,*) 'd1mach test...'
      do i = 1,5
         dans=d1mach(i)
         write(6,'('' d1mach('',i1,'')='',1pd20.13)') i,dans
      enddo
C------------------
      write(6,*) ' ==> portTest:  going to sleep for  2  seconds:'
      write(6,*) '     call gosleep(2)'
      call gosleep(2)
      write(6,*) ' '
C
      call wall_seconds(iwtime2)
      idwall=iwtime2-iwtime1
      write(6,1100) idwall
 1100 format(
     >   ' wall_seconds:  wall clock time elapsed:  ',i3,' seconds.'/)
C------------------
      call cptimer(ztime2)
      zdtime=ztime2-ztime1
      write(6,1099) zdtime
 1099 format(/'cptimer:  cpu time used by porttest:  ',1pe10.3,
     >   ' seconds.'/)
C------------------
#ifdef __DEBUG
c
c  (not safe for exported NTCC test)
c
      write(6,*) ' **** gmkdir tests **** '
      write(6,*) '   $HOME/foo/foo and $CWD/foo/foo are created... '
      call gmkdir(' ','~/foo',ier)
      if(ier.ne.0) then
         write(6,*) ' ?? gmkdir (1) returned ier=',ier
      endif
      call gmkdir(' ',trim(hvalue)//'/foo/foo',ier)
      if(ier.ne.0) then
         write(6,*) ' ?? gmkdir (2) returned ier=',ier
      endif
      call gmkdir(' ','foo/foo',ier)
      if(ier.ne.0) then
         write(6,*) ' ?? gmkdir (3) returned ier=',ier
      endif
      write(6,*) ' **** gmkdir directories not deleted **** '
#endif
c
C------------------
      write(6,*) '...OK, the following is a test of errmsg_exit'
      write(6,*) '...an exit routine that writes a message and'
      write(6,*) '...calls "bad_exit".  "bad_exit" sets an error'
      write(6,*) '...status code for the parent shell.'
C
      call errmsg_exit('  taking bad_exit now...')
C
C------------------
      stop
      end
C---------------------------
      subroutine cvtd_test(isecs)
      integer isecs                     ! no. of seconds
      integer iarray(4)                 ! days/hours/mins/seconds
C
      call cvt_days(isecs,iarray)
      write(6,1001) isecs,(iarray(i),i=1,4)
 1001 format(' ...',i6,' seconds is:  ',i2,' days, ',
     >   i2,' hours, ',i2,' minutes, ',i2,' seconds.'/)
C
      return
      end
