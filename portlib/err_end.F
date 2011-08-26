      Subroutine err_end
C
C  code provided to arrange for portability to SUN with
C  desired floating point error handling
C
#if __SUN || __SUNOS
 
      Integer  ieee_handler
      External ieee_handler
      Integer  ieee_flags
      External ieee_flags
      Integer  istat
      Character*16 str
C
      istat=ieee_flags('clear','exception','all',str)
      If (istat.Ne.0) Stop 'Error in clearing ieee flags'
C
      istat=ieee_handler('clear','division',str)
      If (istat.Ne.0) Stop 'Error in clearing ieee division'
      istat=ieee_handler('clear','invalid' ,str)
      If (istat.Ne.0) Stop 'Error in clearing ieee invalid'
      istat=ieee_handler('clear','overflow',str)
      If (istat.Ne.0) Stop 'Error in clearing ieee overflow'
      Call standard_arithmetic()
#endif
C
#ifndef __NOF90
      call f90_link1
#endif
      return
      End
