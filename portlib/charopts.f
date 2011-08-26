      subroutine charopts(nopts,opts,present,value)
C
C  set flag(s) and fetch value(s) if character argument option(s) is(are)
C  present in the command line, otherwise clear flag(s), set value(s) blank.
C
C  all options are expected, as per the unix convention, to start with "-".
C
C  example:  nopts=1, opts='-file', command line contains "-file xyz.dat"
C     then this routine will return present(1)=.TRUE., value(1)="xyz.dat".
C
C  input:
      integer nopts                     ! number of defined Boolean options
      character*(*) opts(nopts)         ! the option strings-- "-xyz opt_arg"
C
C output:
      logical present(nopts)            ! T if option is found, F otherwise
      character*(*) value(nopts)        ! value of option argument
C
C---------------------
C
      character*128 zarg,zargval
C
C---------------------
C
      do i=1,nopts
         present(i)=.false.
         value(i)=' '
         if(opts(i)(1:1).ne.'-') then
            write(6,*) ' %charopts:  call error, opts(',i,')=',
     >         opts(i),' does not start with "-".'
         endif
      enddo
C
      call get_arg_count(inum)
C
      do ii=1,inum
         call get_arg(ii,zarg)
         if(zarg(1:1).eq.'-') then
            do i=1,nopts
               if(zarg.eq.opts(i)) then
                  present(i)=.true.
                  ip1=ii+1
                  call get_arg(ip1,zargval)
                  if(zargval(1:1).ne.'-') then
                     value(i)=zargval
                  endif
               endif
            enddo
         endif
      enddo
C
      return
      end
