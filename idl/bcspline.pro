;c  bcspline -- dmc 30 May 1996
;c
;c  set up coefficients for bicubic spline with following BC's:
;c  FULL BC CONTROL at all bdys
;c
;      subroutine bcspline(x,inx,th,inth,ffspl,inf3,
;     >                    ibcxmin,bcxmin,ibcxmax,bcxmax,
;     >                    ibcthmin,bcthmin,ibcthmax,bcthmax,
;     >                    wk,nwk,ilinx,ilinth,ier)
;c
;      real x(inx),th(inth),ffspl(4,4,inf3,inth),wk(nwk)
;      real bcxmin(inth),bcxmax(inth)
;      real bcthmin(inx),bcthmax(inx)
;c
;c  input:
;c    x(1...inx) -- abscissae, first dimension of data
;c   th(1...inth) -- abscissae, second (periodic) dimension of data
;c   ffspl(1,1,1..inx,1..inth) -- function values
;c   inf3 -- ffspl dimensioning, inf3.ge.inx required.
;c
;c  boundary conditions input:
;c   ibcxmin -- indicator for boundary condition at x(1):
;c    bcxmin(...) -- boundary condition data
;c     =-1 -- periodic boundary condition
;c     =0 -- use "not a knot", bcxmin(...) ignored
;c     =1 -- match slope, specified at x(1),th(ith) by bcxmin(ith)
;c     =2 -- match 2nd derivative, specified at x(1),th(ith) by 
;c           bcxmin(ith)
;c     =3 -- boundary condition is slope=0 (df/dx=0) at x(1), all th(j)
;c     =4 -- boundary condition is d2f/dx2=0 at x(1), all th(j)
;c     =5 -- match 1st derivative to 1st divided difference
;c     =6 -- match 2nd derivative to 2nd divided difference
;c     =7 -- match 3rd derivative to 3rd divided difference
;c           (for more detailed definition of BCs 5-7, see the
;c           comments of subroutine mkspline)
;c   the bcxmin(...) array is referenced only if ibcxmin=1 or
;c   ibcxmin=2.
;c
;c   ibcxmax -- indicator for boundary condition at x(nx):
;c    bcxmax(...) -- boundary condition data
;c     (interpretation as with ibcxmin, bcxmin)
;c   NOTE:  if ibcxmin=-1, ibcxmax is ignored! ...and the BC is periodic.
;c
;c   ibcthmin -- indicator for boundary condition at th(1):
;c    bcthmin(...) -- boundary condition data
;c     (interpretation as with ibcxmin, bcxmin)
;c   ibcthmax -- indicator for boundary condition at th(inth):
;c    bcthmax(...) -- boundary condition data
;c     (interpretation as with ibcxmin, bcxmin)
;c   NOTE:  if ibcthmin=-1, ibcthmax is ignored! ...the BC is periodic.
;c
;c   NOTE the bcxmin,bcxmax,bcthmin,bcthmax arrays are only used if the
;c     corresponding boundary condition flags are set to 1 or 2.
;c     Carefully note the dimensioning of these arrays!
;c
;c  output:
;c   ffspl(*,*,1..inx,1..inth) -- bicubic spline coeffs (4x4)
;c   ...ffspl(1,1,*,*) is not replaced.
;c
;c   ilinx -- =1 on output if x(inx) pts are nearly evenly spaced
;c            (tol=1e-3)
;c   ilinth-- =1 on output if th(inth) are nearly evenly spaced 
;c            (tol=1e-3)
;c
;c   ier -- completion code, 0 for normal
;c
;c  workspace:
;c   wk -- must be at least 5*max(inx,inth) large *** OR ***
;c         4*inx*inth + 5*max(inx,inth) if ibcthmin=1 or 2
;c         or ibcthmax=1 or 2 and non-zero explicit derivative
;c         boundary information is provided.
;c  nwk -- size of workspace provided.


function bcspline,x,y,f, $
    ixmin=ixmin,fxmin=fxmin,ixmax=ixmax,fxmax=fxmax, $
    iymin=iymin,fymin=fymin,iymax=iymax,fymax=fymax, $
    xlinear=xlin,ylinear=ylin,error=error,status=status

  nx=size(x,/n_elements)
  ny=size(y,/n_elements)
  nmax=max([nx,ny])
  fdim = size(f,/dimensions)
;  print, nx, ny
;  print, 'max =',nmax
;  print, fdim

  if nx NE fdim[0] then begin
    print, "bcspline: size of x != size of f"
    return, 2
  endif
  if ny NE fdim[1] then begin
    print, "bcspline: size of y != size of f"
    return, 2
  endif

  inf3=nx   
  fspl = fltarr(4,4,inf3,ny)
  fspl(0,0,*,*) = f(*,*)
  xlin=0
  ylin=0
  error=long(0)

  if not keyword_set(ixmin) then ixmin=0
  if not keyword_set(fxmin) then fxmin=0.0*y
  if not keyword_set(ixmax) then ixmax=0
  if not keyword_set(fxmax) then fxmax=0.0*y
  if not keyword_set(iymin) then iymin=0
  if not keyword_set(fymin) then fymin=0.0*x
  if not keyword_set(iymax) then iymax=0
  if not keyword_set(fymax) then fymax=0.0*x

  if iymin EQ 1 OR iymin EQ 2 OR iymax EQ 1 or iymax EQ 2 then begin
    wk=fltarr(5*nmax+4*nx*ny)
  endif else begin
    wk=fltarr(5*nmax)
  endelse
  iwk=size(wk,/n_elements)

  status=call_external("libpspline_idl.so","bcspline_idl", $
    x,long(nx),y,long(ny),fspl,long(inf3),     $
    long(ixmin),fxmin,long(ixmax),fxmax, $
    long(iymin),fymin,long(iymax),fymax, $
    wk,long(iwk),long(xlin),long(ylin),long(error))
  if status NE 0 then begin
    print,"bcspline_idl: status = ",status 
  endif
  if error NE 0 then begin
    print,"bcspline_idl: error = ",error 
  endif
  if xlin NE 0 then print,"bcspline_idl: xlinear = ",xlin
  if ylin NE 0 then print,"bcspline_idl: ylinear = ",ylin 

  return, fspl
end
