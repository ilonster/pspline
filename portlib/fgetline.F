      subroutine fgetline(prompt,answer,lenans)
c
c  send prompt, get answer.
c  use C routine (with command line editing) if this is available
c  otherwise revert to fortran i/o
c
      implicit NONE
c
      character*(*) prompt              ! prompt to send (blank for none)
      character*(*) answer              ! answer returned
      integer lenans                    ! non-blank length of answer
c
c  return lenans=-1 on EOF
c
c--------------------------------------------
c
      integer :: status = 0
c
c  0 -- availability of C routine unknown
c  1 -- C routine available
c  2 -- revert to fortran i/o
c
      integer :: ichars
c
      integer str_length
c
c--------------------------------------------
c
#include "fpreproc/byte_declare.h"
c
      byte_declare :: bzero
      byte_declare, allocatable :: bprompt(:)
      byte_declare, allocatable :: banswer(:)
c
c--------------------------------------------
c
      answer=' '
      lenans=0
#if __CRAY || __SX
	bzero = char(0)
#else
	bzero = 0
#endif
c
      if(status.ne.2) then
c
         allocate(banswer(len(answer)+1))
c
         if(prompt.ne.' ') then
            allocate(bprompt(len(prompt)+1))
            call cstring(prompt,bprompt,'2C')
            call getline(bprompt,banswer,ichars)
            deallocate(bprompt)
         else
            call getline(bzero,banswer,ichars)
         endif
c
         if(ichars.eq.-99) then
            if(status.eq.0) write(6,*)
     >         '[fgetline:  no command line editing available]'
            status = 2                  ! dummy C routine
         else
            status = 1
            if(ichars.eq.-1) then
               lenans=-1                ! end of file
            else
c  normal result:  a line is read.
               call cstring(answer,banswer,'2F')
               lenans = str_length(answer)
            endif
         endif
c
         deallocate(banswer)
         if(status.ne.2) return
      endif
c
c------------------------
c  fallthru:  fortran i/o
c
      if(prompt.ne.' ') then
         ichars=str_length(prompt)
         write(6,*) prompt(1:ichars)
      endif
      read(5,'(A)',end=99) answer
      lenans = str_length(answer)
      return
c
 99   continue
      lenans=-1
      return
c
      end
