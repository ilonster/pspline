C***********************************************************************
      SUBROUTINE TERM_OUT(BUFFER,NBYTES)
C
      implicit none
C
#include "fpreproc/byte_declare.h"
C
C...This routine was written by Harry H. Towner of the Princeton
C...Plasma Physics Lab. This routine is similar to John Coonrod's
C...routine JC_LINE. This routine will output NBYTES of array BUFFER
C...to the user's terminal
C
C...Parameters:
C*..BUFFER	- Array of NBYTES to be sent to ther user's terminal.
C*..NBYTES	- The number of bytes to be sent to the terminal
C
C***********************************************************************
C
      integer nbytes
      byte_declare BUFFER(NBYTES)
C
#ifdef __VMS
	INCLUDE '($IODEF)'
	INTEGER SYS$ASSIGN,SYS$QIOW,IOS(2)	
	INTEGER*2 ICHAN	
C	
        integer ifunc,istat
C
C...Define function for passall style output	
C	
	IFUNC=IOR(IO$_WRITEVBLK,IO$M_NOFORMAT)	
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
	ISTAT=SYS$QIOW(,%VAL(ICHAN),%VAL(IFUNC),IOS,,,%REF(BUFFER),	
     >		%VAL(NBYTES),,,,)
C
#endif
C-----------------------------------------------------------------------
C  UNIX CODE
C
#ifdef __UNIX
      INTEGER ITEMP,ISTAT,I,ZPUTC
C
      DO I=1,NBYTES
#if __CRAY || __SX
        ITEMP=ichar(BUFFER(I))
#else	
        ITEMP=BUFFER(I)
#endif	
        ISTAT=ZPUTC(ITEMP)	
      END DO	
C
#endif
      RETURN
      END
C-----------------------
      subroutine term_str_out(str)
      implicit none
C
      character*(*) str
C
      integer NBSIZ
      parameter (NBSIZ=128)
C
      byte_declare BUFFER(NBSIZ)
C
      integer ilen,ic0,ice,inb,i,ic,str_length
C
      ilen=str_length(str)
C
      do ic0=1,ilen,NBSIZ
         ice=min(ilen,(ic0+NBSIZ-1))
         inb=ice-ic0+1
         do i=1,inb
            ic=ic0+i-1
#if __CRAY || __SX
            buffer(i)=str(ic:ic)
#else
            buffer(i)=ichar(str(ic:ic))
#endif
         enddo
C
         call term_out(buffer,inb)
      enddo
C
      return
      end
