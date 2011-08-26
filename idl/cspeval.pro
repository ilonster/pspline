;c  cspeval -- eval cubic spline function and/or derivatives
;c
;      subroutine cspeval(xget,iselect,fval,x,nx,ilinx,f,ier)
;c
;      real xget                         ! interpolation target
;      real fval(3)                      ! output values
;      real x(nx),f(4,nx)                ! spline data
;c
;      integer iselect(3)                ! output selector
;      integer ilinx                     ! =1 if x(...) is evenly spaced
;c
;c  input:
;c     (xget)        location where interpolated value is desired
;c                   x(1).le.xget.le.x(nx) expected
;c
;c     iselect       select desired output
;c
;c                     iselect(1)=1 -- want function value (f) itself
;c                     iselect(2)=1 -- want  df/dx
;c                     iselect(3)=1 -- want  d2f/dx2
;c
;c              example:  iselect(1)=iselect(2)=iselect(3)=1
;c                            f, df/dx, and d2f/dx2 are all evaluated
;c                            and returned in order in fval(1), fval(2),
;c                            and fval(3)
;c                        iselect(1)=0, iselect(2)=1, iselect(3)=0
;c                            only the 1st derivative is evaluated
;c                            and returned in fval(1).
;c
;c                     set iselect(1)=3 to get d3f/dx3, 1 value only.
;c
;c                   see fval (output) description.
;c
;c     x(1...nx)     independent coordinate x, strict ascending
;c
;c     ilinx  --  =1: flag that x is linearly spaced (avoid search for speed)
;c
;c  **CAUTION** actual even spacing of x, is NOT CHECKED HERE!
;c
;c     f             the function values (at grid points) and spline coefs
;c
;c  evaluation formula:  for point x btw x(i) and x(i+1), dx=x-x(i)
;c
;c      spline value =
;c        f(1,i) + dx*f(2,i) + dx**2*f(3,i) + dx**3*f(4,i)
;c
;c  output:
;c      up to 3 elements of fval, ordered as follows:
;c        fval(1)=function value or lowest order derivative requested
;c        fval(2)=next order derivative
;c             etc
;c        the ordering is a subset of the sequence given under the "iselect"
;c        description.
;c
;c      ier = 0 -- successful completion; = 1 -- an error occurred.
;c

function cspeval, x, xspl, fspl, iselect=iselect, xlinear=xlinear, error=error
  NDSPL=2
  NFSPL=4
  NFGET=3

  nxspl = size(xspl,/n_elements)
  ndimfspl=size(fspl,/n_dimensions)
  dimfspl=size(fspl,/dimensions)
  if NDSPL NE ndimfspl OR NFSPL NE dimfspl[0] then begin 
    print, "bcspeval: fspl has incorrect dimensions"
    return,2
  endif
  if nxspl NE dimfspl[1] then begin 
    print, "bcspeval: xspl, fspl have incompatible dimensions"
    return,2
  endif

  if keyword_set(iselect) then begin
    if size(iselect,/n_elements) NE NFGET then begin
        print, "cspeval: iselect must have "+string(NFGET)+" elements"
    endif
    imask = where(iselect GT 0)
    NFSEL = size(imask,/n_elements)
  endif else begin
    iselect = 1+intarr(NFGET)
    imask = where(iselect GT 0)
    NFSEL = NFGET
  endelse
  print, iselect
  print, imask
  print, "NFGET ", NFGET, NFSEL

  nx = size(x,/n_elements)
  xdim = size(x,/dimensions)
  if xdim[0] EQ 0 then xdim=nx
  print, xdim
  f = fltarr([NFSEL,xdim])
  fget=fltarr(NFGET)
  error=long(0)
  print, size(f,/dimensions)

  if not keyword_set(xlinear) then xlinear=0

  for ix=0,nx-1 do begin
    xget=x[ix]
    status=call_external("libpspline_idl.so","cspeval_idl", $
      xget,long(iselect),fget, $
      xspl,long(nxspl),long(xlinear), $
      fspl,error)
    print, ix, fget
    f[NFSEL*ix:NFSEL*(ix+1)-1]=fget[imask]
    if status NE 0 then begin
      print,"cspeval_idl: status = ",status 
    endif
    if error NE 0 then begin
      print,"cspeval_idl: error = ",error 
    endif
 endfor 
 return, f
end
