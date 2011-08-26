      subroutine get_arg(i,arg)
C
C  return i'th argument on the command line.
C
#ifdef __NAGWARE
      use f90_unix_env
#endif
      implicit none
      integer i                         ! argument wanted (in)
      character*(*) arg                 ! i'th argument returned (out)
C
#ifdef __VMS
      call getarg_vms(i,arg)
#endif
 
#ifdef __UNIX
      integer j
 
#if __CRAY || __X1
      integer len,ierr
#endif
 
      j=i
 
c 06/30/00 CAL: not available with f90; is fort77 ?
ccc#if __HP
ccc      call igetarg(j,arg,len(arg))
ccc#elif __CRAY
#if __CRAY || __X1
      call pxfgetarg(j,arg,len,ierr)
#elif __ABS
      call getarg_(j,arg)
#else
      call getarg(j,arg)
#endif
 
#endif /* __UNIX */
 
      call str_pad(arg)
 
      return
      end
 
