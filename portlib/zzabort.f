C----------------------------------------------------------------------
C  ABORT A JOB AFTER A FATAL ERROR CONDITION IS DETECTED
C    FORCE AN ARITHMETIC ERROR WHICH THE JOB CONTROL CODE SHOULD BE
C    ABLE TO DETECT
C
      SUBROUTINE ZZABORT(ZARG)
C
      write(6,1001)
 1001 FORMAT(//' %ZZABORT:  ** ABORT SUBROUTINE CALLED **'//)
C
      call bad_exit
      stop                              ! this stmt not reached
      end
