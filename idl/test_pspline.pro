n = 10
x=findgen(n)
fx=x^3
b=0*x
c=b
d=b
wk=b


status = call_external("libpspline_idl.so","pspline_idl",n,x,fx,b,c,d,wk)
print, status
print, b, c, d, wk

;IDL> print, b, c, d
;      113.706     -27.4706      20.1765      24.7647      48.7647      74.1765      110.529      137.706      226.647      113.706
;     -196.941      55.7647     -8.11765      12.7059      11.2941      14.1176      22.2353      4.94118      84.0000     -196.941
;      84.2353     -21.2941      6.94118    -0.470588     0.941176      2.70588     -5.76471      26.3529     -93.6471      84.2353

status = call_external("libpspline_idl.so","spline_idl",n,x,fx,b,c,d)
print, status
print, b, c, d

fspl = cspline(x,fx)
t = findgen(2*n)/2.0
ft = cspeval(t,x,fspl)

stop

nx=10
ny=5
nz=8
x=findgen(nx)
y=findgen(ny)
z=findgen(nz)
onex = 0*x+1
oney = 0*y+1
fspl = bcspline(x,y,x^2#y,xlin=xlin,ylin=ylin)
feval = bcspeval(2.2,3.3,x,y,fspl,xlin=xlin,ylin=ylin)

feval = bcspeval(x*(1-x/30),y*(1-y/20),x,y,fspl,/grid)

tfun=fltarr(10,5,8)
for iz=0,nz-1 do tfun[*,*,iz] = x^2#y*sqrt(z[iz])


tspl = tcspline(x,y,z,tfun)
teval = tcspeval(x*(1-x/30),y*(1-y/20),z*(1-z/40),x,y,z,tspl)
