      subroutine listopt(list_max,list_cur,list,option)
C
C  fetch list argument values for a selected command line option; append
C  these to list.
C
C  input:  list(1),... list(list_cur) -- the list on input
C          option -- the option code which flags additional list values
C
C  output:  list_cur updated (but not past list_max)
C          list(1),... list(list_cur) -- the updated list.
C
C  list elements are appended in the order found within the command line.
C
C  example:  suppose "-I" flags arguments specifying directories to search
C      for "include" files.  Suppose that on input, list_cur=1 and list(1)='.';
C      option='-I'; list_max=10
C
C      the command line contains "... -I /usr/include  /usr/local/include"
C
C  then... /usr/include and /usr/local/include ... are appended to the list.
C  and list_cur is increased to 3.
C
C
      integer list_max                  ! max list length (input)
      integer list_cur                  ! current list length (input, updated)
      character*(*) list(list_max)      ! list of values (input, updated)
      character*(*) option              ! option flag (input)
C
C---------------------
C
      character*128 zarg
C
C---------------------
C
      if(option(1:1).ne.'-') then
         write(6,*) ' %listopt:  call error, option=',option,
     >      ' does not start with "-".'
         return
      endif
C
      call get_arg_count(inum)
C
      ii=0
 100  continue
C
      ii=ii+1
      if(ii.gt.inum) go to 500
C
C  find option code
C
      call get_arg(ii,zarg)
      if(zarg(1:1).ne.'-') go to 100
      if(zarg.ne.option) go to 100
C
C  found.
C
 120  continue
C
      ii=ii+1
      if(ii.gt.inum) go to 500
C
      call get_arg(ii,zarg)
      if(zarg(1:1).eq.'-') then
         ii=ii-1
         go to 100
      endif
C
C  element found; check for duplication
C
      idup=0
      do i=1,list_cur
         if(zarg.eq.list(i)) idup=1
      enddo
C
      if(idup.eq.0) then
C
C  add to list
C
         if(list_cur.lt.list_max) then
            list_cur=list_cur+1
            list(list_cur)=zarg
         else
            write(6,*) ' %listopt -- max list length = ',list_max,
     >         ' exceeded.'
         endif
      endif
      go to 120
C
 500  continue
      return
      end
 
