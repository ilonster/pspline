      subroutine c9date(zdate)
      character*(*) zdate
C
C  get a 9 character date -- portably -- by calling C code
C  RGA, use 10 character C string so it can be null terminated
C
#include "fpreproc/byte_declare.h"
C
      BYTE_DECLARE z9date(10)
C
      ilen=10
      call c9dat0(z9date,ilen)
C
      call cstring(zdate,z9date,'2F')
C
      return
      end
