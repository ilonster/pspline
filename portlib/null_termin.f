      subroutine null_termin(str)
      character*(*) str
C
C  append a null character to the end of a string
C  ("end" defined as in str_length:  the last non-blank character).
C
      k=str_length(str)
      if(k.lt.len(str)) then
         str(k+1:k+1)=char(0)
      endif
C
      return
      end
