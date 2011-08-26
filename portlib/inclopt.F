      subroutine inclopt(list_max,list_cur,list,tfile)
C
C implement "-I" option -- for list of INCLUDE file search directories.
C
      integer list_max                  ! max list size (input)
      integer list_cur                  ! actual list size (output)
      character*(*) list(list_max)      ! the actual list (output)
      character*(*) tfile               ! "template" file specification
C                                       !   -- skip if blank
C
C the template filename contains a directory specification that is to
C be included on the list.
C
C the list includes the current working directory, the template file's
C directory (if a nonblank filename is specified), and all additional
C specifications given via a -I option on the command line.
C
C--------------------------
C
      character*128 zarg
C
      integer str_length
C
C--------------------------
C  1.  the current working directory
C
      call sget_cwd(zarg)
      list_cur=1
      list(1)=zarg
C
C  2.  tfile directory
C
      if(tfile.ne.' ') then
         ilt=str_length(tfile)
         do ic=ilt,1,-1
            if((tfile(ic:ic).eq.']').or.
     >         (tfile(ic:ic).eq.'>').or.
     >         (tfile(ic:ic).eq.':').or.
     >         (tfile(ic:ic).eq.'/')) then
               list_cur=list_cur+1
               list(list_cur)=tfile(1:ic)
               go to 10
            endif
         enddo
      endif
C
C  3.  supplement list from command line
C
 10   continue
      call listopt(list_max,list_cur,list,'-I')
C
C  be sure that all directory specs are properly decorated...
C
      do id=1,list_cur
         ild=str_length(list(id))
#ifdef __UNIX
         if(list(id)(ild:ild).ne.'/') then
            ic=ild+1
            list(id)(ic:ic)='/'
         endif
#endif
#ifdef __VMS
         if((list(id)(ild:ild).ne.':').and.
     >      (list(id)(ild:ild).ne.']').and.
     >      (list(id)(ild:ild).ne.'>')) then
            ic=ild+1
            list(id)(ic:ic)=':'
         endif
#endif
      enddo
C
      return
      end
