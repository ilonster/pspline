 
      integer function nblkfac(ier)
 
C     NBLKFAC returns the blocking factor for doing an OPEN
C             of a DIRECT access file.
C             For "most" systems, the RECL is given in words.
C             For HP and IBM, RECL is given in bytes.
C             So, for HP and IBM, the number of words must be
C             multiplied by 4 to get the right RECL.
C
C this version uses cpp -- instead of a getenv call for WORKSTATION_TYPE
C
C             Created 12/12/95  T.B. Terpstra
C
c modified D. Ernst           8/98 for LINUX using Portland Group compiler
C          jconboy@jet.uk 15-06-99 change default to 4; better too large
C                                  than too small
 
      Character*20 wstype
 
      ier=1
c--   nblkfac=1
      nblkfac=4
 
#ifdef __VMS
      ier=0
      nblkfac=1
#endif
 
#ifdef __UNIX
#if __ALPHA
      ier=0
      nblkfac=1
#elif __HP || __IBM || __RS6000 || __LINUX || __SUN || __SUNOS || __SGI || __SX || __OSX
      ier=0
      nblkfac=4
#elif __CRAY || __X1
      ier=0
      nblkfac=8
#endif 
#endif  /* __UNIX */
 
      return
      end
