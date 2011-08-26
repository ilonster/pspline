! $Id: eztest.F90,v 1.5 2007/05/04 16:50:45 Ludescher-Furth Exp $
! lf95 -I${LOCAL}/mod eztest.f90 ${LOCAL}/lib/ezcdf.a /usr/local/lib/libnetcdf.a 
! lf95 -I/usr/local/include -I./LINUX/mod eztest.f90 ./LINUX/lib/ezcdf.a /usr/local/lib/libnetcdf.a
! ifc -w -I./LINUX/mod eztest.f90 ./LINUX/lib/ezcdf.a /usr/local/lib/libnetcdf.a 

PROGRAM eztest

  call SampleWrite
  call SampleRead
  call OldSyntaxWrite
  call OldSyntaxRead
  call mixNetcdfEzcdf
  call BigFile

END PROGRAM eztest

subroutine BigFile
  use ezcdf
  implicit none
  integer, parameter :: r8 = selected_real_kind(12,100)
  integer, parameter :: n1=20, n2=40, n3=300
  integer, parameter :: nd1=1, nd2=2, nd3=3
  integer :: ai(n1,n2,n3), bi(n1+nd1,n2+nd2,n3+nd3)
  real :: ar(n1,n2,n3), br(n1+nd1,n2+nd2,n3+nd3)
  real(r8) :: ar8(n1,n2,n3), br8(n1+nd1,n2+nd2,n3+nd3)
  complex :: ac8(n1,n2,n3), bc8(n1+nd1,n2+nd2,n3+nd3)
  complex(r8) :: ac16(n1,n2,n3), bc16(n1+nd1,n2+nd2,n3+nd3)
  integer :: ci(n2,n3), di(n2+nd2,n3+nd3)
  integer :: cr(n2,n3), dr(n2+nd2,n3+nd3)
  integer :: cr8(n2,n3), dr8(n2+nd2,n3+nd3)
  integer :: cc8(n2,n3), dc8(n2+nd2,n3+nd3)
  integer :: cc16(n2,n3), dc16(n2+nd2,n3+nd3)
  real :: tic, tac
  
  integer i, j, k, ncid, no_errors
#ifdef __UNIX
#if __HP || __NAGWARE
      integer :: cpclock
#elif __CRAY || __X1
      real :: second
#elif __ABS
      real :: etime_
      real :: tarray(2)
#else
      real :: etime
      real :: tarray(2)
#endif
#endif  /* __UNIX */
  
  do k = 1, n3
     do j = 1, n2
        ci(j,k) = 10*j + k
        cr(j,k) = 10*j + k + 0.1
        cr8(j,k) = 10*j + k + 0.1_r8
        cc8(j,k) = 10*j + k + 0.1
        cc16(j,k) = 10*j + k + 0.1_r8
        do i = 1, n1
           ai(i,j,k) = 100*i + 10*j + k
           ar(i,j,k) = 100*i + 10*j + k + 0.1
           ar8(i,j,k) = 100*i + 10*j + k + 0.1_r8
           ac8(i,j,k) = 100*i + 10*j + k + 0.1
           ac16(i,j,k) = 100*i + 10*j + k + 0.1_r8
        enddo
     enddo
  enddo
  call cdf_open(ncid, 'bigFile.nc', 'w')
  call cdf_define(ncid, 'ai', ai)
  call cdf_define(ncid, 'ar', ar)
  call cdf_define(ncid, 'ar8', ar8)
  call cdf_define(ncid, 'ac8', ac8)
  call cdf_define(ncid, 'ac16', ac16)
  call cdf_define(ncid, 'ci', ci)
  call cdf_define(ncid, 'cr', cr)
  call cdf_define(ncid, 'cr8', cr8)
  call cdf_define(ncid, 'cc8', cc8)
  call cdf_define(ncid, 'cc16', cc16)
  print *,' writing data ...'
  call cdf_write(ncid, 'ai', ai)
  call cdf_write(ncid, 'ar', ar)
  call cdf_write(ncid, 'ar8', ar8)
  call cdf_write(ncid, 'ac8', ac8)
  call cdf_write(ncid, 'ac16', ac16)
  call cdf_write(ncid, 'ci', ci)
  call cdf_write(ncid, 'cr', cr)
  call cdf_write(ncid, 'cr8', cr8)
  call cdf_write(ncid, 'cc8', cc8)
  call cdf_write(ncid, 'cc16', cc16)
  call cdf_close(ncid)  

  call cdf_open(ncid, 'bigFile.nc', 'r')

#ifdef __UNIX 
#if __FUJITSU || __OSX || __IBM || __RS6000 
      call cpu_time(tic)    ! not portable
#elif __HP || __NAGWARE
      tic = 1.0e-6*cpclock()
#elif __CRAY || __X1
      tic = second()
#elif __ABS
      tic = etime_(tarray)
#else
      tic = etime(tarray)
#endif
 
#endif  /* __UNIX */
!  call cpu_time(tic)
  print *,' reading data ...'
  call cdf_read(ncid, 'ai', bi)
  call cdf_read(ncid, 'ar', br)
  call cdf_read(ncid, 'ar8', br8)
  call cdf_read(ncid, 'ac8', bc8)
  call cdf_read(ncid, 'ac16', bc16)
  call cdf_read(ncid, 'ci', di)
  call cdf_read(ncid, 'cr', dr)
  call cdf_read(ncid, 'cr8', dr8)
  call cdf_read(ncid, 'cc8', dc8)
  call cdf_read(ncid, 'cc16', dc16)
#ifdef __UNIX 
#if __FUJITSU || __OSX || __IBM || __RS6000 
      call cpu_time(tac)    ! not portable
#elif __HP || __NAGWARE
      tac = 1.0e-6*cpclock()
#elif __CRAY || __X1
      tac = second()
#elif __ABS
      tac = etime_(tarray)
#else
      tac = etime(tarray)
#endif
 
#endif  /* __UNIX */

  call cdf_close(ncid)  
  print *,' checking data ...'
  no_errors = 0
  do k = 1, n3
     do j = 1, n2
        if(ci(j,k)/=di(j,k)) no_errors = no_errors+1
        if(cr(j,k)/=dr(j,k)) no_errors = no_errors+1
        if(cr8(j,k)/=dr8(j,k)) no_errors = no_errors+1
        if(cc8(j,k)/=dc8(j,k)) no_errors = no_errors+1
        if(cc16(j,k)/=dc16(j,k)) no_errors = no_errors+1        
        do i = 1, n1
           if(ai(i,j,k) /= bi(i,j,k)) no_errors = no_errors+1
           if(ar(i,j,k) /= br(i,j,k)) no_errors = no_errors+1
           if(ar8(i,j,k) /= br8(i,j,k)) no_errors = no_errors+1
           if(ac8(i,j,k) /= bc8(i,j,k)) then
              print *,i,j,k, ac8(i,j,k), bc8(i,j,k)
              no_errors = no_errors+1
           endif
!!$           if(ac16(i,j,k) /= bc16(i,j,k)) no_errors = no_errors+1
        enddo
     enddo
  enddo
  print *,' cpu time (read): ', tac-tic
  print *, no_errors, ' errors after BigFile'

end subroutine BigFile

subroutine mixNetcdfEzcdf

  ! Mixing ezcdf and netcdf calls

  use ezcdf
  implicit none
!!$  include 'netcdf.inc'

  integer ScalarInt
  integer :: ScalarInt_id, ier, fid
  character(NF_MAX_NAME) :: dim_names(3)

  ! write

  call ezcdf_open(fid, 'EZtest3.nc', 'w', ier)
  call ezcdf_close(fid, ier)

  ! read 

  call ezcdf_open(fid, 'EZtest.nc', 'r', ier)
  ier = NF_INQ_VARID(fid, 'ScalarInt', ScalarInt_id)
  ier = NF_GET_VAR_INT(fid, ScalarInt_id, ScalarInt)
  print *,' ScalarInt = ', ScalarInt
  call ezcdf_close(fid, ier)


  

end subroutine mixNetcdfEzcdf

subroutine OldSyntaxWrite

  use ezcdf
  implicit none
  integer ncid
  call cdfOpn(ncid, 'test2.nc', 'w')
  call cdfDefVar(ncid, 'myvar', (/1,1,1/), 'LOG')
  call cdfPutVar(ncid, 'myvar', .TRUE.)
  call cdfCls(ncid)

end subroutine OldSyntaxWrite

subroutine OldSyntaxRead

  use ezcdf
  implicit none
  integer ncid
  integer dimlens(3)
  character(5) :: xtype
  logical myvar
  call cdfOpn(ncid, 'test2.nc', 'r')
  call cdfInqVar(ncid, 'myvar', dimlens, xtype)
  call cdfGetVar(ncid, 'myvar', myvar)
  if(.not. myvar) stop '1 error in OldSyntaxRead'
  call cdfCls(ncid)

end subroutine OldSyntaxRead
  


subroutine SampleWrite
! Sample to Write / Read netCDF dataset
! C. Ludescher/A. Pletzer Tue Apr  4 10:11:33 EDT 2000
! Added logicals S. Hirshman Oct 2 2002
      USE ezcdf
      IMPLICIT NONE
      integer, PARAMETER :: r8 = SELECTED_REAL_KIND(12,100)
      integer, PARAMETER :: r4 = SELECTED_REAL_KIND(6,37)
! Variables to be written
      character :: a = 'a'
      character(25)                  :: title =                         &
     &                                  "Test of EZcdf Interface"
      character(8), dimension(2)      :: comment =                       &
     &                                  (/"Written ","02/15/99"/)
      character(8), dimension(2)      :: comment_a =                     &
     &                                  (/"Written ","10/02/02"/)
      real (KIND=r8)                 :: scalar = 999.99_r8
      integer                        :: scalar_int = 33
      integer,        dimension(5)   :: ival = (/1,2,3,4,5/)
      logical,        dimension(4)   :: lval = (/.true.,.false.,.false., .true./)
      logical,        dimension(4,2) :: lval2
      real (KIND=r8), dimension(3,4) :: dval =                          &
     &       reshape ( (/11.1_r8, 12.2_r8, 13.3_r8,                     &
     &                   21.1_r8, 22.2_r8, 23.3_r8,                     &
     &                   31.1_r8, 32.2_r8, 33.3_r8,                     &
     &                   41.1_r8, 42.2_r8, 43.3_r8/),                   &
     &                  (/3,4/))
      real (KIND=r4), dimension(3,4,2) :: fval =            &
     &       reshape ( (/                                   &
     &                   1.1, 1.2, 1.3,                     &
     &                   2.1, 2.2, 2.3,                     &
     &                   3.1, 3.2, 3.3,                     &
     &                   4.1, 4.2, 4.3,                     &
     &                   5.1, 5.2, 5.3,                     &
     &                   6.1, 6.2, 6.3,                     &
     &                   7.1, 7.2, 7.3,                     &
     &                   8.1, 8.2, 8.3                      &
     &                  /),                                 &
     &                  (/3,4,2/))
      complex(KIND=r8) :: c16val_0,  c16val_1(5), c16val_2(3,4), c16val_3(3,4,2)
      complex(KIND=r4) :: c8val_0,  c8val_1(5), c8val_2(3,4), c8val_3(3,4,2)
      integer :: intarray(-3:2) = (/ -3, -2, -1, 0, 1, 2/)

! to define Variables
      integer, dimension(3) :: dimlens = (/1,1,1/)
      integer               :: ncid
      integer               :: ier
       
!
!----------------------------------------
! Create File
      call cdf_open(ncid,'EZtest.nc','w',ier)
      if (ier .ne. 0) then
         print *,'Error creating file'
         stop
      end if
! Define Variables
! Title
      dimlens(1)=len(title)
      call cdf_define(ncid, 'a', a)
      call cdf_define(ncid,'Title',dimlens,'CHAR',ier)  ! ier = optional
      call cdf_define(ncid,'Title-A', title)
      if (ier .ne. 0) stop
! Comment
      dimlens(1)=len(comment(1))
      dimlens(2)=size(comment)
      call cdf_define(ncid,'Comment',dimlens,'CHAR')
      call cdf_define(ncid,'Comment-A', comment_a)
! Scalar
      dimlens(1)=1
      dimlens(2)=1
      call cdf_define(ncid,'Scalar',dimlens,'R8')

! 1D-INT
      dimlens(1)=size(ival)
      call cdf_define(ncid,'1D-INT',dimlens,'INT')
      call cdf_define(ncid, '1d_array_with_indexing_starting_at_-3', intarray, ier)
      ! we can set attributes
      call cdf_setatt(ncid, '1D-INT', 'this is the long name of 1D-INT', 'Wb', ier)
      call cdf_define(ncid,'1D-INTa',ival,dimname=(/'radius'/))
      ! without error code
      call cdf_setatt(ncid, '1D-INTa','long name of 1D-INTa', 'm')
      
      dimlens = (/ 1,1,1 /)
      call cdf_define(ncid,'ScalarInt',dimlens,'INT')
      call cdf_define(ncid,'ScalarInta',scalar_int)
! 2D-R8
      dimlens(1)=3
      dimlens(2)=4
      call cdf_define(ncid,'2D-R8',dimlens,'R8')
      call cdf_define(ncid,'2D-R8a',dval,dimname=(/'x_coord','y_coord'/))
! 3D-R4
      dimlens(1)=3
      dimlens(2)=4
      dimlens(3)=2
      call cdf_define(ncid,'3D-R4',dimlens,'R4')
! Complex
      dimlens = 0
      call cdf_define(ncid,'Scalar_C16',dimlens,'C16')
      call cdf_define(ncid,'Scalar_C8',dimlens,'C8')
      dimlens = (/ 5, 1, 1 /)
      call cdf_define(ncid,'1D_C16',dimlens,'C16')
      call cdf_define(ncid,'1D_C8',dimlens,'C8')
      dimlens = (/ 3, 4, 1 /)
      call cdf_define(ncid,'2D_C16',dimlens,'C16')
      call cdf_setatt(ncid,'2D_C16', 'complex data have their name mangled', 'kA')
      call cdf_define(ncid,'2D_C8',dimlens,'C8')
      dimlens = (/ 3, 4, 2 /)
      call cdf_define(ncid,'3D_C16',dimlens,'C16')
      call cdf_define(ncid,'3D_C8',dimlens,'C8')

      c16val_0 = scalar * (1._r8, 2._r8)
      c16val_1 = ival * (1._r8, 2._r8)
      c16val_2 = dval * (1._r8, 2._r8)
      c16val_3 = fval * (1._r8, 2._r8)
      c8val_0 = scalar * (3._r4, 4._r4)
      c8val_1 = ival * (3._r4, 4._r4)
      c8val_2 = dval * (3._r4, 4._r4)
      c8val_3 = fval * (3._r4, 4._r4)
! 1D Logical
      dimlens = (/ size(lval), 1, 1 /)
      call cdf_define(ncid,'1D-LOG',dimlens,'LOG')
      ! units are optional
      call cdf_setatt(ncid,'1D-LOG', 'logicals are stored with appending __logical__')
! 2D Logical
      dimlens = (/ size(lval2,1), size(lval2,2), 1 /)
      call cdf_define(ncid,'2D-LOG',dimlens,'LOG')
      lval2(:,1) = lval
      lval2(:,2) = .not.lval
     
!
! Write Variables
      call cdf_write(ncid, 'a', a)
      call cdf_write(ncid,'Title',title,ier)   ! ier = optional
      call cdf_write(ncid,'Title-A',title,ier)  
      if (ier .ne. 0) stop
      call cdf_write(ncid,'Comment',comment)
      call cdf_write(ncid,'Comment-A',comment_a)
      call cdf_write(ncid,'Scalar',scalar)
      call cdf_write(ncid,'2D-R8',dval)
      call cdf_write(ncid,'2D-R8a',dval)
      call cdf_write(ncid,'3D-R4',fval)
      call cdf_write(ncid,'1D-INT',ival)
      call cdf_write(ncid,'1D-INTa',ival)
      call cdf_write(ncid,'1d_array_with_indexing_starting_at_-3', intarray, ier)
      call cdf_write(ncid,'Scalar_C16',c16val_0)
      call cdf_write(ncid,'1D_C16',c16val_1)
      call cdf_write(ncid,'2D_C16',c16val_2)
      call cdf_write(ncid,'3D_C16',c16val_3)
      call cdf_write(ncid,'Scalar_C8',C8val_0)
      call cdf_write(ncid,'1D_C8',C8val_1)
      call cdf_write(ncid,'2D_C8',C8val_2)
      call cdf_write(ncid,'3D_C8',C8val_3)
      call cdf_write(ncid,'1D-LOG',lval)
      call cdf_write(ncid,'2D-LOG',lval2)

      call cdf_write(ncid,'ScalarInt',scalar_int)
      call cdf_write(ncid,'ScalarInta',scalar_int)

      call cdf_close(ncid ,ier)
!
! Now read back
      call SampleRead

    END subroutine SampleWrite

!========================================================================
      SUBROUTINE SampleRead
      USE ezcdf
      implicit none
      integer, PARAMETER :: r8 = SELECTED_REAL_KIND(12,100)
      integer, PARAMETER :: r4 = SELECTED_REAL_KIND(6,37)
! netCDF data sets:
      real (KIND=r8), dimension(:,:), allocatable :: dval, dval_a
      real (KIND=r4), dimension(:,:,:), allocatable :: fval
      integer,        dimension(:),   allocatable :: ival
      logical,        dimension(:),   allocatable :: lval
      logical,        dimension(:,:), allocatable :: lval2
      integer                                     :: scalarInt, scalarInta
      real (KIND=r8)                              :: scalar
      character :: a
      character(25)                                :: title, title_a
      character(8), dimension(2)                   :: comment, comment_a
      complex(KIND=r8) :: c16val_0
      complex(KIND=r8), allocatable :: c16val_1(:), c16val_2(:,:), &
           & c16val_3(:,:,:)
      complex(KIND=r4) :: c8val_0
      complex(KIND=r4), allocatable :: c8val_1(:), c8val_2(:,:), &
           & c8val_3(:,:,:)
      integer, dimension(-3:2) :: intarray
! Local
      integer                 :: ncid
      integer                 :: ierror
      integer, dimension(3)   :: dimlens = (/1,1,1/)
      character(4)            :: xtype
!
      integer                 :: i, j, k, ner
      character(80) :: longname, units
      integer ier
!----------------------------------------------------------------

      ner = 0
      print "(/'Read again:'/)"
! Open File
      call cdf_open(ncid,'EZtest.nc','r')
! Inquire dimensions and type of Variable
! 1D-INT
      call cdf_inquire(ncid,'1D-INT',dimlens,ier=ierror)  
      ALLOCATE( ival(dimlens(1)), STAT=ierror )
      call cdf_getatt(ncid, '1D-INT', longname, units, ier)
      print *,' -- 1D-INT: ',trim(longname), ' units=', trim(units), ' ier=', ier
      call cdf_inquire(ncid, '1d_array_with_indexing_starting_at_-3', dimlens, xtype)
      print *,' variable 1d_array_with_indexing_starting_at_-3 has dimlens=', dimlens, ' and type ', xtype
! 1D-LOG
      call cdf_inquire(ncid,'1D-LOG',dimlens,ier=ierror)
      call cdf_getatt(ncid, '1D-LOG', longname, units)
      print *,' -- 1D-LOG: ',trim(longname), ' units=', trim(units)
      ALLOCATE( lval(dimlens(1)), STAT=ierror)
! 2D-LOG
      call cdf_inquire(ncid,'2D-LOG',dimlens,ier=ierror)
      ALLOCATE( lval2(dimlens(1), dimlens(2)), STAT=ierror)
! 2D-R8
      call cdf_inquire(ncid,'2D-R8',dimlens)
      ALLOCATE( dval(dimlens(1),dimlens(2)), STAT=ierror )
      call cdf_inquire(ncid,'2D-R8a',dimlens)
      ALLOCATE( dval_a(dimlens(1),dimlens(2)), STAT=ierror )
! 2D-R4
      call cdf_inquire(ncid,'3D-R4',dimlens)
      ALLOCATE( fval(dimlens(1),dimlens(2),dimlens(3)), STAT=ierror )
! Comment
      call cdf_inquire(ncid,'Comment',dimlens)
! Complex
      call cdf_inquire(ncid,'Scalar_C16',dimlens)
      print "('Scalar_C16 dims=',3I4, 2x, a)", dimlens, xtype
      call cdf_inquire(ncid,'1D_C16',dimlens,xtype)
      print "('1D_C16 dims=',3I4, 2x, a)", dimlens, xtype
      allocate(c16val_1(dimlens(1)))
      call cdf_inquire(ncid,'1D_C8',dimlens)
      allocate(c8val_1(dimlens(1)))
      call cdf_inquire(ncid,'2D_C16',dimlens,xtype)
      print "('2D_C16 dims=',3I4, 2x, a)", dimlens, xtype
      allocate(c16val_2(dimlens(1), dimlens(2)))
      call cdf_getatt(ncid, '2D_C16', longname, units, ier)
      print *,' -- 2D_C16: ',trim(longname), ' units=', trim(units), ' ier=', ier
      call cdf_inquire(ncid,'2D_C8',dimlens)
      allocate(c8val_2(dimlens(1), dimlens(2)))
      call cdf_inquire(ncid,'3D_C16',dimlens,xtype)
      print "('3D_C16 dims=',3I4, 2x, a)", dimlens, xtype
      allocate(c16val_3(dimlens(1), dimlens(2), dimlens(3)))
      call cdf_inquire(ncid,'3D_C8',dimlens)
      allocate(c8val_3(dimlens(1), dimlens(2), dimlens(3)))
!
! Get data
      call cdf_read(ncid, 'a', a)
      call cdf_read(ncid,'Title',title,ierror)             ! ierror = optional
      call cdf_read(ncid,'Title-A',title_a,ierror)    
      print *,'Title:   ',title
      print *,'Title-A: ',title_a
      call cdf_read(ncid,'Comment',comment)
      print *,'Comment(1): ',comment(1)
      print *,'Comment(2): ',comment(2)
      call cdf_read(ncid,'Comment-A',comment_a)
      print *,'Comment-A(1): ',comment_a(1)
      print *,'Comment-A(2): ',comment_a(2)
      call cdf_read(ncid,'Scalar',scalar)
      print *,'Scalar:  ',scalar
      call cdf_read(ncid,'1D-INT',ival)
      print *,'1d-INT:  ',ival
      call cdf_read(ncid,'1D-LOG',lval)
      print *,'1d-LOG:  ',lval
      call cdf_read(ncid,'2D-LOG',lval2)
      print *,'2d-LOG:  ',lval2
      call cdf_read(ncid,'2D-R8',dval)
      call cdf_read(ncid,'2D-R8a',dval_a)
      call cdf_read(ncid,'3D-R4',fval)
      call cdf_read(ncid,'Scalar_C16',c16val_0)
      call cdf_read(ncid,'1D_C16',c16val_1)
      call cdf_read(ncid,'2D_C16',c16val_2)
      call cdf_read(ncid,'3D_C16',c16val_3)
      call cdf_read(ncid,'Scalar_C8',C8val_0)
      call cdf_read(ncid,'1D_C8',C8val_1)
      call cdf_read(ncid,'2D_C8',C8val_2)
      call cdf_read(ncid,'3D_C8',C8val_3)

      call cdf_read(ncid,'ScalarInt',scalarInt)
      call cdf_read(ncid,'ScalarInta',scalarInta)

      print *,' Scalar Int = ', scalarInt, ' Alternative form = ', scalarInta

      print *,'2D-R8:'
      do i=1,4
         print *,dval(:,i)
      end do
      print *,'2D-R8a:'
      do i=1,4
         print *,dval_a(:,i)
      end do
      print *,'3D-R4:'
      do i=1,2
         print *,(fval(:,j,i),j=1,4)
      end do

      if(c16val_0 /= scalar * (1._r8, 2._r8)) ner = ner + 1

      do i = 1, size(ival)
         if(c16val_1(i) /= ival(i) * (1._r8, 2._r8)) ner = ner + 1
      enddo

      do j = 1, size(dval, 2)
         do i = 1, size(dval, 1)
            if(c16val_2(i,j) /= dval(i,j) * (1._r8, 2._r8)) ner = ner + 1
         enddo
      enddo

      do k = 1, size(fval, 3)
         do j = 1, size(fval, 2)
            do i = 1, size(fval, 1)
               if(c16val_3(i,j,k) /= fval(i,j,k) * (1._r8, 2._r8)) ner = ner + 1
            enddo
         enddo
      enddo
      print *,ner,' errors occurred while reading in complex data'
      
    END SUBROUTINE SampleRead
