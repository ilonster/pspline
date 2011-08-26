;c  cspline -- dmc 15 Feb 1999
;c
;c  a standard interface to various 1d spline setup routines
;c
;      subroutine cspline(x,nx,fspl,ibcxmin,bcxmin,ibcxmax,bcxmax,
;     >   wk,iwk,ilinx,ier)
;c
;      real x(nx)                  ! x axis (in)
;      real fspl(4,nx)             ! spline data (in/out)
;      integer ibcxmin             ! x(1) BC flag (in, see comments)
;      real bcxmin                 ! x(1) BC data (in, see comments)
;      integer ibcxmax             ! x(nx) BC flag (in, see comments)
;      real bcxmax                 ! x(nx) BC data (in, see comments)
;      real wk(iwk)                ! workspace of size at least nx
;c
;c  workspace only used if "periodic" boundary condition is selected.
;c
;      integer ilinx               ! even spacing flag (out)
;      integer ier                 ! output status, =0 means OK
;c
;c  this routine computes spline coefficients for a 1d spline --
;c  evaluation of the spline can be done by cspeval.for subroutines
;c  or directly by inline code.
;c
;c  the input x axis x(1...nx) must be strictly ascending, i.e.
;c  x(i+1).gt.x(i) is required for i=1 to nx-1.  This is checked and
;c  ier=1 is set and the routine exits if the test is not satisfied.
;c
;c  on output, ilinx=1 is set if, to a reasonably close tolerance, 
;c  all grid spacings x(i+1)-x(i) are equal.  This allows a speedier
;c  grid lookup algorithm on evaluation of the spline.  If on output
;c  ilinx=2, this means the spline x axis is not evenly spaced.
;c
;c  the input data for the spline are given in f[j] = fspl(1,j).  The
;c  output data are the spline coefficients fspl(2,j),fspl(3,j), and
;c  fspl(4,j), j=1 to nx.  The result is a spline s(x) satisfying the
;c  boundary conditions and with the properties
;c
;c     s(x(j)) = f(1,j)
;c     s'(x) is continuous even at the grid points x(j)
;c     s''(x) is continuous even at the grid points x(j)
;c
;c  the formula for evaluation of s(x) is:
;c
;c     let dx = x-x(i), where x(i).le.x.le.x(i+1).  Then,
;c     s(x)=f(1,i) + dx*(f(2,i) +dx*(f(3,i) + dx*f(4,i)))
;c
;c  ==>boundary conditions.  Complete specification of a 1d spline
;c  requires specification of boundary conditions at x(1) and x(nx).
;c
;c  this routine provides 4 options:
;c
;c  ibcxmin=0 | ibcxmax=0 -- this specifies a "not a knot" boundary
;c    condition -- see cubsplb.for.  This is a common way for inferring
;c    a "good" spline boundary condition automatically from data in the
;c    vicinity of the boundary.  (bcxmin | bcxmax are ignored).
;c
;c  ibcxmin=1 | ibcxmax=1 -- boundary condition is to have s'(x(1)) |
;c    s'(x(nx)) match the passed value (bcxmin | bcxmax).
;c
;c  ibcxmin=2 | ibcxmax=2 -- boundary condition is to have s''(x(1)) | 
;c    s''(x(nx)) match the passed value (bcxmin | bcxmax).
;c
;c  ibcxmin=3 | ibcxmax=3 -- boundary condition is to have s'(x(1)) |
;c    s'(x(nx)) = 0.0 (ZERO)
;c
;c  ibcxmin=4 | ibcxmax=4 -- boundary condition is to have s''(x(1)) | 
;c    s''(x(nx)) = 0.0 (ZERO)
;c
;c  ibcxmin=5 | ibcxmax=5 == BC is to have s' match the 1st divided
;c    difference at the endpoint.
;c
;c  ibcxmin=6 | ibcxmax=6 == BC is to have s'' match the 2nd divided
;c    difference at the endpoint.
;c
;c  ibcxmin=7 | ibcxmax=7 == BC is to have s''' match the 3rd divided
;c    difference at the endpoint.

function cspline,x,f,$
    ixmin=ixmin,fxmin=fxmin,ixmax=ixmax,fxmax=fxmax,$
    xlinear=xlinear,error=error,status=status

  nx=size(x,/n_elements)
  nf=size(f,/n_elements)

  if nx NE nf then begin
    print, "cspline: size of x != size of f"
    return, 2
  endif
   
  fspl = fltarr(4,nx)
  fspl(0,*) = f

  if not keyword_set(ixmin) then ixmin=0
  if not keyword_set(fxmin) then fxmin=0.0
  if not keyword_set(ixmax) then ixmax=0
  if not keyword_set(fxmax) then fxmax=0.0

  wk=fltarr(nx)
  iwk=size(wk,/n_elements)
  xlinear=0
  error=long(0)

  status=call_external("libpspline_idl.so","cspline_idl",$
    x,long(nx),fspl, $
    long(ixmin),fxmin,long(ixmax),fxmax, $
    wk,long(iwk),xlinear,error)
  if status NE 0 then begin
    print,"cspline_idl: status = ",status 
  endif
  if error NE 0 then begin
    print,"cspline_idl: error = ",error 
  endif
  if xlinear NE 0 then begin
    print,"cspline_idl: xlinear = ",xlinear
  endif

  return, fspl
end
