#include "fpreproc/f77_dcomplx.h"
      subroutine r8f3test(fun1,fun2,id,inump,
     >   xmin,xmax,ymin,ymax,zmin,zmax,
     >   name,namex,namey,namez)
c
c   test/compare 2 methods of evaluation of 3d function values and derivatives
c
c     passed:
c       subroutine fun1(x,y,z,fget1(1..10))
c       subroutine fun2(x,y,z,fget2(1..10))
c
c          (input x,y, return vector of 6 numbers containing, in order:
c     f,df/dx,df/dy,df/dz,d2f/dx2,d2f/dy2,d2f/dz2,d2f/dxdy,d2f/dxdz,d2f/dydz)
c
!============
! idecl:  explicitize implicit INTEGER declarations:
      IMPLICIT NONE
      INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
      INTEGER icyc,i,ict,ifvals,iz,iy,ix
!============
! idecl:  explicitize implicit REAL declarations:
      REAL*8 zz,zy,zx,zdifa
!============
      external fun1,fun2
c
      integer id                        ! 1 for 1st derivs, 2 for 2nd derivs.
      integer inump                     ! #evals per dimension
c
      REAL*8 xmin,xmax                    ! x range
      REAL*8 ymin,ymax                    ! y range
      REAL*8 zmin,zmax                    ! z range
c
      character*(*) name                ! name/label for fcns --
      character*(*) namex,namey,namez   ! name/label for x & y dims
c
c  NOTE `name*' should be short with no leading or trailing blanks
c
c  local:
c
      REAL*8 fget1(10),fget2(10),fmin(10),fmax(10),fdifa(10)
c
c  output of routine:  stats written to unit 6
c
c--------------------------------
c
      data icyc/100000/
c
      do i=1,10
         fmin(i)=1.0E35_r8
         fmax(i)=-1.0E35_r8
         fdifa(i)=0.0_r8
      enddo
c
      ict=0
c
      if(id.eq.2) then
         ifvals=10
      else
         ifvals=4
      endif
c
      do iz=1,inump
         zz=zmin+AREAL(iz-1)*(zmax-zmin)/AREAL(inump-1)
         do iy=1,inump
            zy=ymin+AREAL(iy-1)*(ymax-ymin)/AREAL(inump-1)
            do ix=1,inump
               zx=xmin+AREAL(ix-1)*(xmax-xmin)/AREAL(inump-1)
c
               ict=ict+1
               if(icyc*(ict/icyc).eq.ict) then
                  write(6,1001) ix,zx,iy,zy,iz,zz
 1001             format(
     >               '  ...f3test at x(',i5,')=',1pe11.4,
     >               ', y(',i5,')=',1pe11.4,', z(',i5,')=',1pe11.4)
               endif
c
               call fun1(zx,zy,zz,fget1)
               call fun2(zx,zy,zz,fget2)
c
               do i=1,ifvals
                  fmin(i)=min(fmin(i),fget1(i),fget2(i))
                  fmax(i)=max(fmax(i),fget1(i),fget2(i))
                  zdifa=abs(fget1(i)-fget2(i))
                  fdifa(i)=max(fdifa(i),zdifa)
               enddo
c
            enddo
         enddo
      enddo
c
      write(6,1002) namex,namey,namez,name,
     >   (name,fmin(i),fmax(i),fdifa(i),i=1,4)
 1002 format(/
     >'  test function comparison:'/
     >'     x stands for "',a,'";  y stands for "',a,
     >           '"; z stands for "',a/
     >'     (',a,')     min value   max value   max |diff|'/
     >'   ',a,':      ',3(1pe11.3,2x)/
     >'  d',a,'/dx:   ',3(1pe11.3,2x)/
     >'  d',a,'/dy:   ',3(1pe11.3,2x)/
     >'  d',a,'/dz:   ',3(1pe11.3,2x))
      if(ifvals.gt.4) write(6,1003)
     >   (name,fmin(i),fmax(i),fdifa(i),i=5,10)
 1003 format(
     >' d2',a,'/dx2:  ',3(1pe11.3,2x)/
     >' d2',a,'/dy2:  ',3(1pe11.3,2x)/
     >' d2',a,'/dz2:  ',3(1pe11.3,2x)/
     >' d2',a,'/dxdy: ',3(1pe11.3,2x)/
     >' d2',a,'/dxdz: ',3(1pe11.3,2x)/
     >' d2',a,'/dydz: ',3(1pe11.3,2x))
c
      return
      end
