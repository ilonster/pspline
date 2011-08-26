;c
;c  bcspeval -- eval bicubic spline function and/or derivatives
;c
;      subroutine bcspeval(xget,yget,iselect,fval,
;     >                    x,nx,y,ny,ilinx,iliny,f,inf3,ier)
;c
;      integer iselect(6)
;      integer ilinx,iliny,nx,ny,inf3,ier
;c
;      real xget,yget
;      real fval(*)
;      real x(nx),y(ny),f(4,4,inf3,ny)
;c
;c  input:
;c     (xget,yget)   location where interpolated value is desired
;c                   x(1).le.xget.le.x(nx) expected
;c                   y(1).le.yget.le.y(ny) expected
;c
;c     iselect       select desired output
;c
;c                     iselect(1)=1 -- want function value (f) itself
;c                     iselect(2)=1 -- want  df/dx
;c                     iselect(3)=1 -- want  df/dy
;c                     iselect(4)=1 -- want  d2f/dx2
;c                     iselect(5)=1 -- want  d2f/dy2
;c                     iselect(6)=1 -- want  d2f/dxdy
;c
;c              example:  iselect(1)=iselect(2)=iselect(3)=1
;c                            f, df/dx, and df/dy all evaluated
;c                        iselect(4)=iselect(5)=iselect(6)=0
;c                            2nd derivatives not evaluated.
;c
;c                   the number of non zero values iselect(1:6)
;c                   determines the number of outputs...
;c                   see fval (output) description.
;c
;c  new dmc December 2005 -- access to higher derivatives (even if not
;c  continuous-- but can only go up to 3rd derivatives on any one coordinate.
;c     if iselect(1)=3 -- want 3rd derivatives
;c          iselect(2)=1 for d3f/dx3
;c          iselect(3)=1 for d3f/dx2dy
;c          iselect(4)=1 for d3f/dxdy2
;c          iselect(5)=1 for d3f/dy3
;c               number of non-zero values iselect(2:5) gives no. of outputs
;c     if iselect(1)=4 -- want 4th derivatives
;c          iselect(2)=1 for d4f/dx3dy
;c          iselect(3)=1 for d4f/dx2dy2
;c          iselect(4)=1 for d4f/dxdy3
;c               number of non-zero values iselect(2:4) gives no. of outputs
;c     if iselect(1)=5 -- want 5th derivatives
;c          iselect(2)=1 for d5f/dx3dy2
;c          iselect(3)=1 for d5f/dx2dy3
;c               number of non-zero values iselect(2:3) gives no. of outputs
;c     if iselect(1)=6 -- want 6th derivatives
;c          d6f/dx3dy3 -- one value is returned.
;c
;c     x(1...nx)     independent coordinate x, strict ascending
;c     y(1...ny)     independent coordinate y, strict ascending
;c
;c     ilinx  --  =1: flag that x is linearly spaced (avoid search for speed)
;c     iliny  --  =1: flag that y is linearly spaced (avoid search for speed)
;c
;c  **CAUTION** actual even spacing of x, y is NOT CHECKED HERE!
;c
;c
;c     f             the function values (at grid points) and spline coefs
;c
;c  evaluation formula:  for point x btw x(i) and x(i+1), dx=x-x(i)
;c                             and y btw y(j) and y(j+1), dy=y-y(j),
;c
;c      spline value =
;c        f(1,1,i,j) + dx*f(2,1,i,j) + dx**2*f(3,1,i,j) + dx**3*f(4,1,i,j)
;c   +dy*(f(1,2,i,j) + dx*f(2,2,i,j) + dx**2*f(3,2,i,j) + dx**3*f(4,2,i,j))
;c   +d2*(f(1,3,i,j) + dx*f(2,3,i,j) + dx**2*f(3,3,i,j) + dx**3*f(4,3,i,j))
;c   +d3*(f(1,4,i,j) + dx*f(2,4,i,j) + dx**2*f(3,4,i,j) + dx**3*f(4,4,i,j))
;c
;c      where d2=dy**2 and d3=dy**3.
;c
;c  output:
;c      up to 6 elements of fval, ordered as follows:
;c        fval(1)=function value or lowest order derivative requested
;c        fval(2)=next order derivative
;c             etc
;c        the ordering is a subset of the sequence given under the "iselect"
;c        description.
;c
;c      ier = 0 -- successful completion; = 1 -- an error occurred.
;c

function bcspeval, x, y, xspl, yspl, fspl, $
    iselect=iselect, xlinear=xlinear, ylinear=ylinear, grid=grid, error=error
  NDSPL=4
  NFSPL=4
  NFGET=6

  nxspl = size(xspl,/n_elements)
  nyspl = size(yspl,/n_elements)
  ndimfspl = size(fspl,/n_dimensions)
  dimfspl  = size(fspl,/dimensions)
  inf3 = dimfspl[2]
  print, nxspl, nyspl
  print, dimfspl

  if NDSPL NE ndimfspl OR NFSPL NE dimfspl[0] OR NFSPL NE dimfspl[1] then begin 
    print, "bcspeval: fspl has incorrect dimensions"
    return,2
  endif
  if nxspl NE dimfspl[2] OR nyspl NE dimfspl[3] then begin 
    print, "bcspeval: xspl, yspl, fspl have incompatible dimensions"
    return,2
  endif


  nx = size(x,/n_elements)
  dimx = size(x,/dimensions)
  if dimx EQ 0 then dimx=nx
  ny = size(y,/n_elements)
  dimy = size(y,/dimensions)
  if dimy EQ 0 then dimy=ny
 
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
  print, "iselect", iselect
  print, "imask", imask

  if keyword_set(grid) then begin
    f = fltarr([NFSEL,nx,ny])
  endif else begin
    if nx EQ ny then begin
      f = fltarr([NFSEL,dimx])
    endif else begin
      print, "bcspeval: size of x and y are incompatible, is this a grid?"
    endelse
  endelse
  fget=fltarr(NFGET)
  error=long(0)

  if not keyword_set(xlinear) then xlinear=0
  if not keyword_set(ylinear) then ylinear=0

  if keyword_set(grid) then begin
    for ix = 0,nx-1 do begin
    for iy = 0,ny-1 do begin
      xget = x[ix]
      yget = y[iy]
      status=call_external("libpspline_idl.so","bcspeval_idl", $
        xget,yget,long(iselect),fget, $
        xspl,long(nxspl),yspl,long(nyspl),long(xlinear),long(ylinear), $
        fspl,long(inf3), error)
      f[*,ix,iy]=fget[imask]
;      print, ix, xget, iy, yget
;      print, 'fget', fget

      if status NE 0 then begin
        print,"bcspeval_idl: status = ",status 
      endif
      if error NE 0 then begin
        print,"bcspeval_idl: error = ",error 
      endif
   endfor
   endfor
 endif else begin
   for ix=0,nx-1 do begin
      xget = x[ix]
      yget = y[ix]
      status=call_external("libpspline_idl.so","bcspeval_idl", $
        xget,yget,long(iselect),fget, $
        xspl,long(nxspl),yspl,long(nyspl),long(xlinear),long(ylinear), $
        fspl,long(inf3), error)
        f[*,ix]=fget[imask]
;       f[NFSEL*ix:NFSEL*(ix+1)-1]=fget[imask]
;      print,  ix,' xget', xget, yget
;      print, 'fget', fget

      if status NE 0 then begin
        print,"bcspeval_idl: status = ",status 
      endif
      if error NE 0 then begin
        print,"bcspeval_idl: error = ",error 
      endif
   endfor 
 endelse

; print, iselect
 return, f
end
