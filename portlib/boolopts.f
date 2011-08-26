      subroutine boolopts(nopts,opts,present)
C
C  set flag(s) if boolean option(s) is(are) present in the command
C  line, otherwise clear flag(s)
C
C  all options are expected, as per the unix convention, to start with "-".
C
C  example:  nopts=1, opts="-toggle" command line contains "-toggle"
C            then present(1)=TRUE is returned.
C
C  input:
      integer nopts                     ! number of defined Boolean options
      character*(*) opts(nopts)         ! the option strings-- "-xyz"
C
C output:
      logical present(nopts)            ! T if option is found, F otherwise
C
C---------------------
C
      character*128 zarg
C
C---------------------
C
      do i=1,nopts
         present(i)=.false.
         if(opts(i)(1:1).ne.'-') then
            write(6,*) ' %boolopts:  call error, opts(',i,')=',
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
               if(zarg.eq.opts(i)) present(i)=.true.
            enddo
         endif
      enddo
C
      return
      end
