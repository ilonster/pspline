      SUBROUTINE TERM_input(jchar)
      implicit none
C
#include "fpreproc/byte_declare.h"
C
C...This routine was written by Harry H. Towner of the Princeton
C...Plasma Physics Lab.
C
C   Get a character from the terminal
C   --> DMC Oct 1991 <-- UNIX code using ordinary FORTRAN i/o
C                        which requires a carriage return
C
C...Parameters:
C*..jchar	- The byte that was input.
C
C***********************************************************************
C
      byte_declare jchar
C
#ifdef __VMS
      INCLUDE '($IODEF)'
      INTEGER SYS$ASSIGN,SYS$QIOW,IOS(2)
      INTEGER*2 ICHAN
C
C...Define function for passall style output
C
      integer ifunc,istat
C
      IFUNC=IOR(IO$_readVBLK,IO$M_Noecho)
C
      DATA ICHAN/0/
C
C***********************************************************************
C
C...Obtain channel if necessary.
C
      IF(ICHAN.EQ.0) THEN
        ISTAT=SYS$ASSIGN('TT',ICHAN,,)
      END IF
C
      ISTAT=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOS,,,%REF(jchar),
     >		%VAL(1),,,,)
#endif
C
C  UNIX code:
#if __CRAY || __SX
      integer ibuf,zgetc
 
      ibuf = zgetc()
      jchar=char(ibuf)
#elif __UNIX
      integer ibuf,zgetc
 
      ibuf = zgetc()
      jchar=ibuf
#endif
C
      RETURN
      END
C--------------------------------
C
C  read in a single character from the terminal
C
      subroutine term_char_in(achar)
      implicit none
      character*1 achar
C
      byte_declare jchar
C
      call term_input(jchar)
#if __CRAY || __SX
      achar=jchar
#else
      achar=char(jchar)
#endif
C
      return
      end
 
