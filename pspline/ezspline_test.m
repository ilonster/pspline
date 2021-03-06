%
% a matlab program to display the interpolated data generated by
% the ezspline test run.
%
% prerequisite: the mexcdf package should be installed
%
% A. Pletzer 23 March 2000
%
% Usage: 
% matlab
% >> drive
%
addpath /usr/local/mexcdf
ncstartup

disp(' ')
disp('This matlab script graphically represents interpolated data obtained by')
disp('applying various combinations of boundary conditions and interpolation')
disp('methods (either spline or Akima). The Akima method is a variant of the')
disp('Hermite interpolation that does not require derivative data to be supplied')
disp('(they are internally evaluated). Exact data values are provided for comparison.')
disp('Cloud interpolations are represented as a sequence of (x,y) plots.')
disp('Array interpolation will be represented by either surface of isosurface')
disp('plots. Unless otherwise specified, a solid line corresponds to the original')
disp('data, circles (o) to spline and (+) to Akima interpolation. ')
disp(' ')


figure(1)
nc=netcdf('spline1_not_.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
nc=netcdf('spline1_notx.nc', 'nowrite');
variables=var(nc); y1=variables{2}(:); g=variables{3}(:);
close(nc);
myxes=[min(y1), max(y1), min(g), max(g)];

subplot(3,2,1), plot(y1, g, 'b-', x1, f, 'ro'), axis(myxes), 
title('spline 1 not-a-knot')


nc=netcdf('spline1_per_.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
subplot(3,2,2), plot(y1, g, 'b-', x1, f, 'ro'), axis(myxes), 
title('spline 1 periodic')


nc=netcdf('spline1_1st_.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
subplot(3,2,3), plot(y1, g, 'b-', x1, f, 'ro'), axis(myxes), 
title('spline 1 1st derivative')


nc=netcdf('spline1_2nd_.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
subplot(3,2,4), plot(y1, g, 'b-', x1, f, 'ro'), axis(myxes), 
title('spline 1 2nd derivative')

nc=netcdf('akima1__not.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
subplot(3,2,5), plot(y1, g, 'b-', x1, f, 'r+'), axis(myxes),
title('akima 1')


nc=netcdf('akima1__per.nc', 'nowrite');
variables=var(nc); x1=variables{2}(:); f=variables{3}(:);
close(nc);
subplot(3,2,6), plot(y1, g, 'b-', x1, f, 'r+'), axis(myxes),
title('akima 1 per')


%%
%% 2-D data
%%

figure(2)
nc=netcdf('spline2_not__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); f=variables{4}(:);
close(nc);
nc=netcdf('spline2_notx_c.nc', 'nowrite');
variables=var(nc); 
y1=variables{2}(:); y2=variables{3}(:); g=variables{4}(:);
close(nc);
myxes1=[min(y1), max(y1), min(g), max(g)];
myxes2=[min(y2), max(y2), min(g), max(g)];


subplot(4,2,1), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('spline not a knot')
subplot(4,2,2), plot(y2,g,'b-',x2,f,'ro'), axis(myxes2), xlabel('x2')
title('spline not a knot')


nc=netcdf('spline2_per__c.nc', 'nowrite');
variables=var(nc); 
y1=variables{2}(:); y2=variables{3}(:); g=variables{4}(:);
close(nc);
subplot(4,2,3), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('spline per')
subplot(4,2,4), plot(y2,g,'b-',x2,f,'ro'), axis(myxes2), xlabel('x2')
title('spline per')

nc=netcdf('spline2_mix__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); f=variables{4}(:);
close(nc);
subplot(4,2,5), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('spline mixed')
subplot(4,2,6), plot(y2,g,'b-',x2,f,'ro'), axis(myxes2), xlabel('x2')
title('spline mixed')

nc=netcdf('akima2_not__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); f=variables{4}(:);
close(nc);
subplot(4,2,7), plot(y2,g,'b-',x2,f,'r+'), axis(myxes2), xlabel('x2')
title('Akima')

nc=netcdf('akima2_per__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); f=variables{4}(:);
close(nc);
subplot(4,2,8), plot(y2,g,'b-',x2,f,'r+'), axis(myxes2), xlabel('x2')
title('Akima per')



figure(3)
nc=netcdf('spline2_not__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{3}(:); x2=variables{4}(:); f=variables{5}(:);
close(nc);
nc=netcdf('spline2_notx_a.nc', 'nowrite');
variables=var(nc); 
y1=variables{3}(:); y2=variables{4}(:); g=variables{5}(:);
gmax = max(max(g)); gmin = min(min(g));
x1max = max(x1); x1min=min(x1); x2max=max(x2); x2min=min(x2);
myxes = [x1min, x1max, x2min, x2max, gmin, gmax];
close(nc);
subplot(3,2,1), surf(y1, y2, g), title('spline exact'), axis(myxes)
subplot(3,2,2), surf(x1, x2, f), title('spline not a knot'), axis(myxes)
nc=netcdf('spline2_per__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{3}(:); x2=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,2,3), surf(x1, x2, f), title('periodic'), axis(myxes)
nc=netcdf('spline2_mix__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{3}(:); x2=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,2,4), surf(x1, x2, f), title('spline mixed'), axis(myxes)
nc=netcdf('akima2_not__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{3}(:); x2=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,2,5), surf(x1, x2, f), title('Akima 2'), axis(myxes)
nc=netcdf('akima2_per__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{3}(:); x2=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,2,6), surf(x1, x2, f), title('Akima 2 per'), axis(myxes)


figure(4)
nc=netcdf('spline3_mix__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{4}(:); x2=variables{5}(:); x3=variables{6}(:); f=variables{7}(:);
close(nc);
nc=netcdf('spline3_mix_ax.nc', 'nowrite');
variables=var(nc); 
y1=variables{4}(:); y2=variables{5}(:); y3=variables{6}(:); g=variables{7}(:);
close(nc);
gmax = max(max(max(g))); gmin = min(min(min(g)));
x1max=max(x1); x1min=min(x1); 
x2max=max(x2); x2min=min(x2);
x3max=max(x3); x3min=min(x3);

const = 2.5234578;
subplot(2,2,1)
p = patch(isosurface(y1,y2,y3,g,const));isonormals(y1,y2,y3,g,p);
set(p, 'FaceColor', 'red', 'EdgeColor', 'none');daspect([1 1 0.5])
view(3), camlight, lighting phong, title('spline exact (array interp)')

subplot(2,2,2)
p = patch(isosurface(x1,x2,x3,f,const));isonormals(x1,x2,x3,f,p);
set(p, 'FaceColor', 'red', 'EdgeColor', 'none');daspect([1 1 0.5])
view(3), camlight, lighting phong, title('spline mixed 1 (array interp)')

nc=netcdf('spline3_mox__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{4}(:); x2=variables{5}(:); x3=variables{6}(:); f=variables{7}(:);
close(nc);

subplot(2,2,3)
p = patch(isosurface(x1,x2,x3,f,const));isonormals(x1,x2,x3,f,p);
set(p, 'FaceColor', 'red', 'EdgeColor', 'none');daspect([1 1 0.5])
view(3), camlight, lighting phong, title('spline mixed 2')

nc=netcdf('akima3_mix__a.nc', 'nowrite');
variables=var(nc); 
x1=variables{4}(:); x2=variables{5}(:); x3=variables{6}(:); f=variables{7}(:);
close(nc);

subplot(2,2,4)
p = patch(isosurface(x1,x2,x3,f,const));isonormals(x1,x2,x3,f,p);
set(p, 'FaceColor', 'red', 'EdgeColor', 'none');daspect([1 1 0.5])
view(3), camlight, lighting phong, title('akima mixed')


figure(5)
nc=netcdf('spline3_mix_cx.nc', 'nowrite');
variables=var(nc); 
y1=variables{2}(:); y2=variables{3}(:); y3=variables{4}(:); g=variables{5}(:);
close(nc);
y1min=min(y1); y2min=min(y2); y3min=min(y3); gmin=min(g);
y1max=max(y1); y2max=max(y2); y3max=max(y3); gmax=max(g);
myxes1=[y1min, y1max, gmin, gmax]; 
myxes2=[y2min, y2max, gmin, gmax];
myxes3=[y3min, y3max, gmin, gmax];

nc=netcdf('spline3_mix__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); x3=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,1,1), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('spline mixed 1')

nc=netcdf('spline3_mox__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); x3=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,1,2), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('spline mixed 2')

nc=netcdf('akima3_mix__c.nc', 'nowrite');
variables=var(nc); 
x1=variables{2}(:); x2=variables{3}(:); x3=variables{4}(:); f=variables{5}(:);
close(nc);
subplot(3,1,3), plot(y1,g,'b-',x1,f,'ro'), axis(myxes1), xlabel('x1')
title('akima mixed')

