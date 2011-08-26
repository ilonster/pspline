      subroutine c24date(zdate)
      character*(*) zdate
C
C  get a 24 character date -- portably -- by calling C code
C
#include "fpreproc/byte_declare.h"
C
      BYTE_DECLARE z24date(24)
C
      ilen=24
      call c24dat0(z24date,ilen)
C
      call cstring(zdate,z24date,'2F')
C
      return
      end
