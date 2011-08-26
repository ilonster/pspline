      subroutine chkargs(min_num,max_num,args,
     >   max_bool,bool_list,
     >   max_a1op,a1op_list,
     >   max_lsop,lsop_list,
     >   ierr)
C
C  check unix style command line arguments -- simplified context.
C
C  context is that the command line can have three types of items:
C     arguments
C     boolean options  -xyz                            ... bool_list
C     1-argument options   -abc [argument]             ... a1op_list
C     list options  -jik arg1 arg2 arg3 ... -          ... lsop_list
C        (list options can have zero or more arguments, lists are
C        terminated by the next -option or a single "-"
C
C  the number of arguments (not associated with a - option) should be
C  between min_num and max_num; any boolean option should be in bool_list
C  (elements 1 to max_bool); any option with character argument should
C  be on a1op_list (elements 1 to max_a1op) and the argument must in fact
C  be present.
C
C
C ** input **
C
      integer min_num                   ! minimum no. of args not incl. options
      integer max_num                   ! max no. of args not incl. options
C
      character*(*) args(*)             ! the arguments found
C
      integer max_bool                  ! no. of defined boolean options
      character*(*) bool_list(*)        ! list of option codes (w/ "-" char.)
C
      integer max_a1op                  ! no. of defined char arg. options
      character*(*) a1op_list(*)        ! list of option codes (w/ "-" char.)
C
      integer max_lsop                  ! no. of defined char arg. options
      character*(*) lsop_list(*)        ! list of option codes (w/ "-" char.)
C
C ** output **
C
      integer ierr                      ! exit code, 0=OK
C
C-----------------------------------------------------
C  local:
C
      character*128 zarg
C
C-----------------------------------------------------
C
      ierr=0
C
      igot=0
C
      call get_arg_count(inum)
C
      iarg=0
 100  continue
      iarg=iarg+1
      if(iarg.gt.inum) go to 500
C
      call get_arg(iarg,zarg)
C
      if(zarg.eq.'-') go to 100
C
      if(zarg(1:1).eq.'-') then
C  option; must be on one of the lists...
         do ii=1,max_lsop
            if(zarg.eq.lsop_list(ii)) go to 110 ! check list arguments
         enddo
C
         do ii=1,max_a1op
            if(zarg.eq.a1op_list(ii)) go to 150 ! check single argument
         enddo
C
         do ii=1,max_bool
            if(zarg.eq.bool_list(ii)) go to 100 ! automatic OK if on this list
         enddo
C
C  not on any list
C
         ierr=1
         go to 900
C
 110     continue
         iarg=iarg+1
         if(iarg.gt.inum) go to 500
         call get_arg(iarg,zarg)
         if(zarg(1:1).eq.'-') then
            iarg=iarg-1
            go to 100
         else
            go to 110
         endif
C
 150     continue
         if(iarg.eq.inum) then
            ierr=1                      ! last item => missing argument
            go to 900
         endif
         iarg=iarg+1
         call get_arg(iarg,zarg)
         if(zarg(1:1).eq.'-') then
            ierr=1                      ! argument must be non-option
            go to 900
         endif
C
      else
C
C  regular argument (not associated with a -option).
C
         igot=igot+1
         if(igot.le.max_num) args(igot)=zarg
      endif
C
C  next argument item on command line...
C
      go to 100
C
C----------------------------
C  end of list
C
 500  continue
      if((min_num.le.igot).and.(igot.le.max_num)) then
         ierr=0
      else
         ierr=1
      endif
C
C  exit
C
 900  continue
      return
      end
