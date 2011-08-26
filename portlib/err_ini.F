      Subroutine err_ini
C
C  code provided to arrange for portability to SUN with
C  desired floating point error handling
C
C  on newer SUN systems use compiler switches "-fns -ftrap=common" instead
C
#if __SUN || __SUNOS
 
#define SIGFPE_ABORT    %val(2) /* abort on exception */
 
      Integer  ieee_handler
      External ieee_handler
      Integer  ieee_flags
      External ieee_flags
      Integer  istat
C
      istat=ieee_handler('set','division',SIGFPE_ABORT)
      If (istat.Ne.0) Stop 'Error in setting ieee division'
      istat=ieee_handler('set','invalid' ,SIGFPE_ABORT)
      If (istat.Ne.0) Stop 'Error in setting ieee invalid'
      istat=ieee_handler('set','overflow',SIGFPE_ABORT)
      If (istat.Ne.0) Stop 'Error in setting ieee overflow'
      Call nonstandard_arithmetic()
#endif
      return
      End
