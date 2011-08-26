#include "fpreproc/f77_dcomplx.h"
      subroutine r8f2test(fun1,fun2,inump,xmin,xmax,ymin,ymax,
     >   name,namex,namey)
c
c   test/compare 2 methods of evaluation of 2d function values and derivatives
c
c     passed:
c       subroutine fun1(x,y,fget1(1..6))
c       subroutine fun1(x,y,fget2(1..6))
c
c          (input x,y, return vector of 6 numbers containing, in order:
c            f,df/dx,df/dy,d2f/dx2,d2f/dy2,d2f/dxdy)
c
!============
! idecl:  explicitize implicit INTEGER declarations:
      IMPLICIT NONE
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
      INTEGER icyc,i,ict,iy,ix
!============
! idecl:  explicitize implicit REAL declarations:
      REAL*8 zy,zx,zdifa
!============
      external fun1,fun2
c
      integer inump                     ! #evals per dimension
c
      REAL*8 xmin,xmax                    ! x range
      REAL*8 ymin,ymax                    ! y range
c
      character*(*) name                ! name/label for fcns --
      character*(*) namex,namey         ! name/label for x & y dims
c
c  NOTE `name*' should be short with no leading or trailing blanks
c
c  local:
c
      REAL*8 fget1(6),fget2(6),fmin(6),fmax(6),fdifa(6)
c
c  output of routine:  stats written to unit 6
c
c--------------------------------
c
      data icyc/100000/
c
      do i=1,6
         fmin(i)=1.0E35_r8
         fmax(i)=-1.0E35_r8
         fdifa(i)=0.0_r8
      enddo
c
      ict=0
c
      do iy=1,inump
         zy=ymin+AREAL(iy-1)*(ymax-ymin)/AREAL(inump-1)
         do ix=1,inump
            zx=xmin+AREAL(ix-1)*(xmax-xmin)/AREAL(inump-1)
c
            ict=ict+1
            if(icyc*(ict/icyc).eq.ict) then
               write(6,1001) ix,zx,iy,zy,ict
 1001          format('  ...f2test: evaluating at x(',i5,')=',1pe11.4,
     >            ', y(',i5,')=',1pe11.4,' #=',i8)
            endif
c
            call fun1(zx,zy,fget1)
            call fun2(zx,zy,fget2)
c
            do i=1,6
               fmin(i)=min(fmin(i),fget1(i),fget2(i))
               fmax(i)=max(fmax(i),fget1(i),fget2(i))
               zdifa=abs(fget1(i)-fget2(i))
               fdifa(i)=max(fdifa(i),zdifa)
            enddo
c
         enddo
      enddo
c
      write(6,1002) namex,namey,name,
     >   (name,fmin(i),fmax(i),fdifa(i),i=1,6)
 1002 format(/
     >'  test function comparison:'/
     >'     x stands for "',a,'";  y stands for "',a,'".'/
     >'     (',a,')     min value   max value   max |diff|'/
     >'   ',a,':      ',3(1pe11.3,2x)/
     >'  d',a,'/dx:   ',3(1pe11.3,2x)/
     >'  d',a,'/dy:   ',3(1pe11.3,2x)/
     >' d2',a,'/dx2:  ',3(1pe11.3,2x)/
     >' d2',a,'/dy2:  ',3(1pe11.3,2x)/
     >' d2',a,'/dxdy: ',3(1pe11.3,2x)/)
c
      return
      end
