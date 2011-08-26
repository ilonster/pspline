      subroutine sget_env(varname,value)
C
C  get the value of an environment variable (UNIX) or logical name (VMS)
C
C  14Mar2005   jim.conboy@jet.uk
C              if __IDL6_FIX, use cget_env ; w/around for name conflict between
C              IDL Vn 6 & Lahey/F90 getenv functions
C
#ifdef __NAGWARE
      use f90_unix_env
#endif
C
      implicit none
C
C  input:
      character*(*) varname             ! name to be translated
C  output:
      character*(*) value               ! translation returned
C
C  local:
#if __CRAY || __NAGWARE || __VMS
      integer length,ierr
#endif
#if __IBM || __RS6000 || __IDL6_FIX || __X1
      integer iret, cget_env
#endif
C
C  value=' ' on exit, if the environment variable (logical name) is
C  undefined.
C
      integer ilvar,str_length
C
      ilvar=max(1,str_length(varname))
C
#if __IBM || __RS6000 || __IDL6_FIX || __X1
      iret = cget_env(varname(1:ilvar),value)
      if( iret.eq.0 ) value=' '
#elif __CRAY
      call pxfgetenv(varname(1:ilvar),0,value,length,ierr)
      if(ierr.ne.0) value=' '
#elif __NAGWARE
      call getenv(varname(1:ilvar),value,length,ierr)
      if(ierr.ne.0) value=' '
#elif __VMS
      call utrnlog(' ',varname(1:ilvar),value,length,ierr)
      if(ierr.ne.1) value=' '   ! 1 denotes success in VMS utrnlog
#elif __ABS
      call getenv_(varname(1:ilvar),value)
#else
      call getenv(varname(1:ilvar),value)
#endif
      call str_pad(value)
      return
      end
