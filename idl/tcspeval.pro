;c     tcspeval -- eval tricubic spline function and/or derivatives
;c
;      subroutine tcspeval(xget,yget,zget,iselect,fval,
;     >                    x,nx,y,ny,z,nz,ilinx,iliny,ilinz,f,inf4,inf5,
;     >                    ier)
;c
;      integer iselect(10)
;      integer ilinx,iliny,ilinz,nx,ny,nz,inf4,inf5,ier
;c
;      real xget,yget,zget
;      real fval(*)
;      real x(nx),y(ny),z(nz),f(4,4,4,inf4,inf5,nz)
;c
;c  modification -- dmc 11 Jan 1999 -- remove SAVE stmts; break routine
;C    into these parts:
;C
;C    tcspevxyz -- find grid cell of target pt.
;C    tcspevfn -- evaluate function using output of tcspevxyz
;C
;C    in cases where multiple functions are defined on the same grid,
;C    time can be saved by using tcspevxyz once and then tcspevfn
;C    multiple times.
;c
;c  input:
;c     (xget,yget,zget)   location where interpolated value is desired
;c                   x(1).le.xget.le.x(nx) expected
;c                   y(1).le.yget.le.y(ny) expected
;c                   z(1).le.zget.le.z(nz) expected
;c
;c     iselect       select desired output
;c
;c                     iselect(1)=1 -- want function value (f) itself
;c                     iselect(2)=1 -- want  df/dx
;c                     iselect(3)=1 -- want  df/dy
;c                     iselect(4)=1 -- want  df/dz
;c                     iselect(5)=1 -- want  d2f/dx2
;c                     iselect(6)=1 -- want  d2f/dy2
;c                     iselect(7)=1 -- want  d2f/dz2
;c                     iselect(8)=1 -- want  d2f/dxdy
;c                     iselect(9)=1 -- want  d2f/dxdz
;c                     iselect(10)=1 -- want  d2f/dydz
;c
;c
;c              example:  iselect(1)=iselect(2)=iselect(3)=iselect(4)=1
;c                            f, df/dx, df/dy, and df/dz all evaluated
;c                        iselect(5)=iselect(6)=iselect(7)=0
;c                        iselect(8)=iselect(9)=iselect(10)=0
;c                            2nd derivatives not evaluated.
;c
;c  (new dmc Dec 2005 -- higher derivatives available)
;c    iselect(1)=3 --> 3rd derivative, .le.2 diff. in any coordinate
;c      iselect(2:8) select: fxxy fxxz fxyy fxyz fxzz fyyz fyzz
;c      ->note iselect(1)=3, iselect(5)=1 gives fxyz = d3f/dxdydz
;c    iselect(1)=-3 --> 3rd derivative, 3 in one coordinate
;c      iselect(2:4) select: fxxx fyyy fzzz
;c    iselect(1)=4 --> 3rd derivative, .le.2 diff. in any coordinate
;c      iselect(2:7) select: fxxyy fxxyz fxxzz fxyyz fxyzz fyyzz
;c    iselect(1)=-4 --> 3rd derivative, 3 in one coordinate
;c      iselect(2:7) select: fxxxy fxxxz fxyyy fxzzz fyyyz fyzzz
;c    iselect(1)=5 --> 3rd derivative, .le.2 diff. in any coordinate
;c      iselect(2:4) select: fxxyyz fxxyzz fxyyzz
;c    iselect(1)=-5 --> 3rd derivative, 3 in one coordinate
;c      iselect(2:10) select:  fxxxyy fxxxyz fxxxzz fxxyyy fxxzzz
;c                             fxyyyz fxyzzz fyyyzz fzzzyy
;c    iselect(1)=6 --> 3rd derivative, .le.2 diff. in any coordinate
;c      fxxyyzz
;c    iselect(1)=-6 --> 3rd derivative, 3 in one coordinate
;c      iselect(2:10) select: fxxxyyy fxxxyyz fxxxyzz fxxxyyz
;c                            fxxyyyz fxxyzzz fxyyyzz fxyyzzz fyyyzzz
;c    iselect(1)=-7 --> 7th derivative
;c      iselect(2:7) select: fxxxyyyz fxxxyyzz fxxxyzzz
;c                           fxxyyyzz fxxyyzzz fxyyyzzz
;c    iselect(1)=-8 --> 8th derivative
;c      iselect(2:4) select: fxxxyyyzz fxxxyyzzz fxxyyyzzz
;c    iselect(1)=-9 --> 9th derivative:  fxxxyyyzzz
;c
;c-------
;c
;c     x(1...nx)     independent coordinate x, strict ascending
;c     y(1...ny)     independent coordinate y, strict ascending
;c     z(1...nz)     independent coordinate y, strict ascending
;c
;c     ilinx  --  =1: flag that x is linearly spaced
;c
;c                   see fval (output) description.
;c
;c     x(1...nx)     independent coordinate x, strict ascending
;c     y(1...ny)     independent coordinate y, strict ascending
;c     z(1...nz)     independent coordinate y, strict ascending
;c
;c     ilinx  --  =1: flag that x is linearly spaced (avoid search for speed)
;c     iliny  --  =1: flag that y is linearly spaced (avoid search for speed)
;c     ilinz  --  =1: flat that z is linearly spaced (avoid search for speed)
;c
;c  **CAUTION** actual even spacing of x, y, z is NOT CHECKED HERE!
;c
;c
;c     f             the function values (at grid points) and spline coefs
;c
;c  evaluation formula:  for point x btw x(i) and x(i+1), dx=x-x(i)
;c                             and y btw y(j) and y(j+1), dy=y-y(j),
;c                             and z btw z(k) and z(k+1), dz=z-z(k)
;c
;c  do m=1,4
;c   p(m) =
;c    f(1,1,m,i,j,k)+dx*f(2,1,m,i,j,k)+dx**2*f(3,1,m,i,j,k)+dx**3*f(4,1,m,i,j,k)
;c   +dy*(
;c   f(1,2,m,i,j,k)+dx*f(2,2,m,i,j,k)+dx**2*f(3,2,m,i,j,k)+dx**3*f(4,2,m,i,j,k))
;c   +dy**2*(
;c   f(1,3,m,i,j,k)+dx*f(2,3,m,i,j,k)+dx**2*f(3,3,m,i,j,k)+dx**3*f(4,3,m,i,j,k))
;c   +dy**3*(
;c   f(1,4,m,i,j,k)+dx*f(2,4,m,i,j,k)+dx**2*f(3,4,m,i,j,k)+dx**3*f(4,4,m,i,j,k))
;c  enddo
;c  answer = p(1)+dz*p(2)+dz**2*p(3)+dz**3*p(4)
;c
;c      where d2=dy**2 and d3=dy**3.
;c
;c  nb dmc Feb 1999 -- p loops unrolled, by hand, to aid vector compilers
;c
;c  output:
;c      up to 10 elements of fval, ordered as follows:
;c        fval(1)=function value or lowest order derivative requested
;c        fval(2)=next order derivative
;c             etc
;c        the ordering is a subset of the sequence given under the "iselect"
;c        description; the first M elements of fval are used, where M = the
;c        number of non-zero elements of iselect.
;c
;c      ier = 0 -- successful completion; = 1 -- an error occurred.
;c


function tcspeval, x, y, z, xspl, yspl, zspl, fspl, $
    iselect=iselect, xlinear=xlinear, ylinear=ylinear, zlinear=zlinear, grid=grid, error=error
  NDSPL=6
  NFSPL=4
  NFGET=10

  nxspl = size(xspl,/n_elements)
  nyspl = size(yspl,/n_elements)
  nzspl = size(zspl,/n_elements)
  ndimfspl = size(fspl,/n_dimensions)
  dimfspl  = size(fspl,/dimensions)
  inf4 = dimfspl[3]
  inf5 = dimfspl[4]
  print, nxspl, nyspl, nzspl
  print, dimfspl

  if NDSPL NE ndimfspl OR NFSPL NE dimfspl[0] OR NFSPL NE dimfspl[1] OR NFSPL NE dimfspl[2] then begin 
    print, "tcspeval: fspl has incorrect dimensions"
    return,2
  endif
  if nxspl NE dimfspl[3] OR nyspl NE dimfspl[4] OR nzspl NE dimfspl[5] then begin 
    print, "bcspeval: xspl, yspl, zspl, fspl have incompatible dimensions"
    return,2
  endif


  nx = size(x,/n_elements)
  dimx = size(x,/dimensions)
  if dimx EQ 0 then dimx=nx
  ny = size(y,/n_elements)
  dimy = size(y,/dimensions)
  if dimy EQ 0 then dimy=ny
  nz = size(z,/n_elements)
  dimz = size(z,/dimensions)
  if dimz EQ 0 then dimz=nz
 
  if keyword_set(iselect) then begin
    if size(iselect,/n_dimensions) NE NFGET then begin
        print, "iselect has incorrect dimensions"
    endif
    imask = where(iselect GT 0)
    NFSEL = size(imask,/n_elements)
  endif else begin
    iselect=1+intarr(NFGET)
    imask = where(iselect GT 0)
    NFSEL = NFGET
  endelse
  print, iselect
  print, imask

  if keyword_set(grid) then begin
    f = fltarr([NFSEL,nx,ny,nz])
  endif else begin
    if nx EQ ny AND ny EQ nz then begin
      f = fltarr([NFSEL,nx])
    endif else begin
      print, "tcspeval: size of x, y and z are incompatible, is this a grid?"
    endelse
  endelse
  fget=fltarr(NFGET)
  error=long(0)

  if not keyword_set(xlinear) then xlinear=0
  if not keyword_set(ylinear) then ylinear=0
  if not keyword_set(zlinear) then zlinear=0

  if keyword_set(grid) then begin
    for ix = 0,nx-1 do begin
    for iy = 0,ny-1 do begin
    for iz = 0,nz-1 do begin
      xget = x[ix]
      yget = y[iy]
      zget = z[iz]
      status=call_external("libpspline_idl.so","tcspeval_idl", $
        xget,yget,zget,long(iselect),fget, $
        xspl,long(nxspl),yspl,long(nyspl),zspl,long(nzspl), $
        long(xlinear),long(ylinear),long(zlinear), $
        fspl,long(inf4),long(inf5),error)
      f[*,ix,iy,iz]=fget[imask]
;      print, ix, fget

      if status NE 0 then begin
        print,"tcspeval_idl: status = ",status 
      endif
      if error NE 0 then begin
        print,"tcspeval_idl: error = ",error 
      endif
   endfor
   endfor
   endfor
 endif else begin
   for ix=0,nx-1 do begin
      xget = x[ix]
      yget = y[ix]
      zget = z[ix]
      status=call_external("libpspline_idl.so","tcspeval_idl", $
        xget,yget,zget,long(iselect),fget, $
        xspl,long(nxspl),yspl,long(nyspl),zspl,long(nzspl), $
        long(xlinear),long(ylinear),long(zlinear), $
        fspl,long(inf4),long(inf5),error)
      f[*,ix]=fget[imask]
;      print, ix, fget

      if status NE 0 then begin
        print,"tcspeval_idl: status = ",status 
      endif
      if error NE 0 then begin
        print,"tcspeval_idl: error = ",error 
      endif
   endfor 
 endelse

 return, f
end
