;c  tcspline -- dmc 20 Jan 1999
;c
;c  set up coefficients for bicubic spline with following BC's:
;c  * LHS and RHS handled as in cubspl.for for 1st coordinate
;c  * derivatives periodic in second coordinate (use pspline.for)
;c
;c workspace:
;c  if phi bdy cond. is periodic, not-a-knot, df/dphi = 0 everywhere,
;c  or d2f/dphi2 = 0 everywhere, then the phi boundary condition is
;c  "linear" and a workspace of size at least:
;c
;c     nwk = 20*inx*inth + 10*max(inx,inth,inph)
;c
;c  will suffice.
;c
;c  if the phi bdy cond. involves specification of df/dphi .ne. 0 or
;c  d2f/dphi .ne. 0 at any (x,theta) grid point, then, the phi boundary
;c  condition is "non-linear", a correction step is needed, and a workspace
;c  of size at least:
;c
;c     nwk = 16*inx*inth*inph
;c
;c  is required.
;c
;      subroutine tcspline(x,inx,th,inth,ph,inph,fspl,inf4,inf5,
;     >                    ibcxmin,bcxmin,ibcxmax,bcxmax,inb1x,
;     >                    ibcthmin,bcthmin,ibcthmax,bcthmax,inb1th,
;     >                    ibcphmin,bcphmin,ibcphmax,bcphmax,inb1ph,
;     >                    wk,nwk,ilinx,ilinth,ilinph,ier)
;c
;      real x(inx),th(inth),ph(inph)
;      real fspl(4,4,4,inf4,inf5,inph),wk(nwk)
;      real bcxmin(inb1x,*),bcxmax(inb1x,*) ! inth x inph defined (if used)
;      real bcthmin(inb1th,*),bcthmax(inb1th,*) ! inx x inph defined (if used)
;      real bcphmin(inb1ph,*),bcphmax(inb1ph,*) ! inx x inth defined (if used)
;c
;c  input:
;c    x(1...inx) -- abscissae, first dimension of data
;c   th(1...inth) -- abscissae, second (periodic) dimension of data
;c   ph(1...inph) -- abscissae, third (periodic) dimension of data
;c   fspl(1,1,1,1..inx,1..inth,1..inph) -- function values
;c   inf4 -- fspl dimensioning, inf4.ge.inx required.
;c   inf5 -- fspl dimensioning, inf5.ge.inth required.
;c
;c  boundary conditions input:
;c
;c   bc data at xmin, xmax  vs.  theta,phi
;c   bc data at thmin, thmax  vs.  x,phi
;c   bc data at phmin, phmax  vs.  x,theta
;c
;c   ibcxmin -- indicator for boundary condition at x(1):
;c    bcxmin(...) -- boundary condition data
;c     =-1 -- use periodic boundary condition
;c     =0 -- use "not a knot", bcxmin(...) ignored
;c     =1 -- match slope, specified at x(1),th(ith),ph(iph) by bcxmin(ith,iph)
;c     =2 -- match 2nd derivative, specified at x(1),th(ith),ph(iph)
;c           by bcxmin(ith,iph
;c     =3 -- boundary condition is slope=0 (df/dx=0) at x(1), all th(j)
;c     =4 -- boundary condition is d2f/dx2=0 at x(1), all th(j)
;c     =5 -- match 1st derivative to 1st divided difference
;c     =6 -- match 2nd derivative to 2nd divided difference
;c     =7 -- match 3rd derivative to 3rd divided difference
;c           (for more detailed definition of BCs 5-7, see the
;c           comments of subroutine mkspline)
;c   NOTE bcxmin(...) referenced ONLY if ibcxmin=1 or ibcxmin=2
;c
;c   ibcxmax -- indicator for boundary condition at x(nx):
;c    bcxmax(...) -- boundary condition data
;c     (interpretation as with ibcxmin, bcxmin)
;c     NOTE:  if ibcxmin=-1 then the periodic BC applies on both sides
;c            and ibcxmax is ignored.
;c   inb1x -- 1st dimension of bcxmin, bcxmax: if ibcxmin or ibcxmax .gt. 0
;c            this must be .ge. inth:
;c
;c   interpretation of ibcthmin,bcthmin,ibcthmax,bcthmax,inb1th
;c     is same as with ibcxmin,...
;c
;c   interpretation of ibcphmin,bcphmin,ibcphmax,bcphmax,inb1ph
;c     is same as with ibcxmin,...
;c
;c   the explicit bdy condition arrays are referenced only if the
;c     corresponding "ibc" flag values are set to 1 or 2.
;c
;c  output:
;c   fspl(*,*,*,1..inx,1..inth,1..inph) -- bicubic spline coeffs (4x4)
;c   ...fspl(1,1,1,*,*,*) is not replaced.
;c
;c   ilinx -- =1 on output if x(inx) pts are nearly evenly spaced (tol=1e-3)
;c   ilinth-- =1 on output if th(inth) evenly spaced (tol=1e-3)
;c   ilinph-- =1 on output if ph(inph) evenly spaced (tol=1e-3)
;c
;c   ier -- completion code, 0 for normal
;c
;c  workspace:
;c   wk -- must be at least 5*max(inx,inth,inph) large -- or more, see
;c         comments, above.
;c  nwk -- size of workspace


function tcspline,x,y,z,f, $
    ixmin=ixmin,fxmin=fxmin,ixmax=ixmax,fxmax=fxmax, $
    iymin=iymin,fymin=fymin,iymax=iymax,fymax=fymax, $
    izmin=izmin,fzmin=fzmin,izmax=izmax,fzmax=fzmax, $
    xlinear=xlin,ylinear=ylin,zlinear=zlin,error=error,status=status

  nx=size(x,/n_elements)
  ny=size(y,/n_elements)
  nz=size(z,/n_elements)
  nmax=max([nx,ny,nz])
  fdim = size(f,/dimensions)
  print, nx, ny, nz
  print, 'max =',nmax
  print, fdim

  if nx NE fdim[0] then begin
    print, "tcspline: size of x != size of f"
    return, 2
  endif
  if ny NE fdim[1] then begin
    print, "tcspline: size of y != size of f"
    return, 2
  endif
  if nz NE fdim[2] then begin
    print, "tcspline: size of z != size of f"
    return, 2
  endif

  inf4=nx
  inf5=ny   
  fspl = fltarr(4,4,4,inf4,inf5,nz)
  fspl(0,0,0,*,*,*) = f(*,*,*)
  xlin=0
  ylin=0
  zlin=0
  error=long(0)

  if not keyword_set(ixmin) then ixmin=0
  if not keyword_set(fxmin) then fxmin=fltarr(ny,nz)
  if not keyword_set(ixmax) then ixmax=0
  if not keyword_set(fxmax) then fxmax=fltarr(ny,nz)
  nfxmin = size(fxmin,/dimensions)
  nfxmax = size(fxmax,/dimensions)
  if nfxmin[0] NE nfxmax[0] then begin
    print, "tcspline: size of fxmin != size of fxmax"
    return, 2
  endif
  if nfxmin[0] LT ny then begin
    print, "tcspline: size of fxmin,max < ny"
    return, 2
  endif

  if not keyword_set(iymin) then iymin=0
  if not keyword_set(fymin) then fymin=fltarr(nz,nx)
  if not keyword_set(iymax) then iymax=0
  if not keyword_set(fymax) then fymax=fltarr(nz,nx)
  nfymin = size(fymin,/dimensions)
  nfymax = size(fymax,/dimensions)
  if nfymin[0] NE nfymax[0] then begin
    print, "tcspline: size of fymin != size of fymax"
    return, 2
  endif
  if nfymin[0] LT nz then begin
    print, "tcspline: size of fymin,max < nz"
    return, 2
  endif

  if not keyword_set(izmin) then izmin=0
  if not keyword_set(fzmin) then fzmin=fltarr(nx,ny)
  if not keyword_set(izmax) then izmax=0
  if not keyword_set(fzmax) then fzmax=fltarr(nx,ny)
  nfzmin = size(fzmin,/dimensions)
  nfzmax = size(fzmax,/dimensions)
  if nfzmin[0] NE nfzmax[0] then begin
    print, "tcspline: size of fzmin != size of fzmax"
    return, 2
  endif
  if nfzmin[0] LT nx then begin
    print, "tcspline: size of fzmin,max < nx"
    return, 2
  endif

  if izmin EQ 1 OR izmin EQ 2 OR izmax EQ 1 or izmax EQ 2 then begin
    wk=fltarr(10*nmax+20*nx*ny)
  endif else begin
    wk=fltarr(16*nx*ny*nz)
  endelse
  iwk=size(wk,/n_elements)

  status=call_external("libpspline_idl.so","tcspline_idl", $
    x,long(nx),y,long(ny),z,long(nz),fspl,long(inf4),long(inf5), $
    long(ixmin),fxmin,long(ixmax),fxmax,long(nfxmin[0]), $
    long(iymin),fymin,long(iymax),fymax,long(nfymin[0]), $
    long(izmin),fzmin,long(izmax),fzmax,long(nfzmin[0]), $
    wk,long(iwk),long(xlin),long(ylin),long(zlin),error)
  if status NE 0 then begin
    print,"tcspline_idl: status = ",status 
  endif
  if error NE 0 then begin
    print,"tcspline_idl: error = ",error 
  endif
  if xlin NE 0 then begin
    print,"tcspline_idl: xlinear = ",xlin 
  endif
  if ylin NE 0 then begin
    print,"tcspline_idl: ylinear = ",ylin 
  endif
  if zlin NE 0 then begin
    print,"tcspline_idl: zlinear = ",zlin 
  endif
 
  return, fspl
end
