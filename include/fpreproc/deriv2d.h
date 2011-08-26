/* -*-C++-*- */

#ifndef __deriv2d__
#define __deriv2d__

#include<string>

#include "Vector.h"
#include "Matrix.h"
#include "cubspline.h"


extern "C"  void lagp_( double *y, double  *fy, int *ny, int *order,
                        double *yi, double *dum, double *fip,
                        int *one, int *per, int *ier );


/******************************************************************************

derivative functions on 2-d grid 

******************************************************************************/


Mat dylag( Vec &y, Mat &f, int order=4, int per=1) {

/*
  derivative with respect to FIRST index using Lagrangian interpolation
     order >=3
     per = 1 if periodic grid
*/
  if( order < 3 ) {
    cerr << "dylag WARNING order < 3 will reset to 3 \n";
    order = 3;
  }
  double fip, dum;

  int one = 1;
  int ier = 0, ercount = 0;
  size_t ny = f.size(0);
  if ( y.size() != f.size(0) ) {
    cerr << "dylag ERROR sizes " << y.size() << " and " << ny <<
            " are incompatible \n";
    exit(1);
  }
  size_t nx = f.size(1);
  Mat d(f.size(0), nx);
  Vec dy(f.size(0));
  Vec fy(f.size(0));
  Vec_int I = space(static_cast<size_t>(0), ny-1, ny);
  int ny_int = static_cast<int>(ny);
  for(size_t j=0; j<nx; ++j) {
    fy = f(I, j);
    for (size_t i=0; i<f.size(0); ++i) {
      lagp_( &y(0), &fy(0), &ny_int, &order, &y(i), 
	&dum, &fip, &one, &per, &ier);

      d(i,j) = fip;
      if( ier !=0 ) ercount++;
    }
  }
  if( ercount!=0 ) {
    cerr << "dylag WARNING: " << ercount <<
                     " interpolations outside interval \n";
  }
return d;
}
 
 
Mat dxlag( Vec &x, Mat &f, int order=4, int per=0) {
/*
  derivative with respect to SECOND index using Lagrangian interpolation
     order >=3
     per = 1 if periodic grid
 */
if( order < 3 ) {
    cerr << "dxlag WARNING order < 3 will reset to 3 \n";
    order = 3;
  }
  double fip, dum;
  int one = 1;
  int ier = 0, ercount = 0;
  size_t nx = f.size(1);
  if ( x.size() != f.size(1)  ) {
    cerr << "dxlag ERROR sizes " << x.size() << " and " << nx <<
            " are incompatible \n";
    exit(1);
  }
  size_t ny = f.size(0);
  Mat d(ny, f.size(1));
  Vec dx(f.size(1));
  Vec fx(f.size(1));
  Vec_int J(nx);
  J.range(0);

  int nx_int = static_cast<int>(nx);
  for(size_t i=0; i<ny; ++i) {
    fx = f(i, J);
    for (size_t j=0; j<f.size(1); ++j) {
      lagp_( &x(0), &fx(0), &nx_int, 
	     &order, &x(j), &dum, &fip, &one, &per, &ier );
      d(i,j) = fip;
      if( ier !=0 ) ercount++;
    }
  }
  if( ercount!=0 ) cerr << "dxlag WARNING: " << ercount <<
                     " interpolations outside interval \n";
return d;
}

void reg_near_axis( Mat &inout, Vec &x, int npoints=4 ) {

/*
  regularize near axis
*/

  size_t ntheta = inout.size(0);

  double average = 0.0;
  double *xp = new double[npoints];
  double *fp = new double[npoints];
  double f0, dum;
  int zero = 0, per = 0, ier = 0;

  for (size_t i=0; i<ntheta; ++i) {
    for (int j=0; j<npoints; ++j) {
      xp[j] = x(1+j);
      fp[j] = inout(i,1+j);
    }

  lagp_( &xp[0], &fp[0], &npoints, &npoints, &x(0), &f0, 
    &dum, &zero, &per, &ier);
  average += f0;
  }

  average /= ntheta;

  for (unsigned i=0; i<ntheta; ++i) inout(i,0) = average;

  delete[] xp;
  delete[] fp;
}   


Mat dy( Vec &y, Mat &f, std::string method="spline") {
/*
  derivative with respect to FIRST index
*/

  size_t ny = f.size(0);
  if ( y.size() != ny ) {
    cerr << "dy ERROR sizes " << y.size() << " and " << ny <<
            " are incompatible \n";
    exit(1);
  }
  size_t nx = f.size(1);
  Mat d(ny, nx);

  switch (method[0]){
   
  case 'l':
    d = dylag( y, f ); // periodic BCs
    break;

  default:

  Vec dy(ny);
  Vec fy(ny);
  Vec_int I(ny);
  I.range(0); // integer array of size ny and starting at 0
  for(size_t j=0; j<nx; ++j) {
    fy = f(I, j);
    cubspline fc(y, fy, "periodic" ); // periodic BC's
    dy = fc.prime( y );
    d.slip( dy, I, j);
    // for (size_t i=0; i<ny; ++i) d(i,j) = dy(i);
  }

  }
  reg_near_axis( d, y, 4 );
  return d;
}

 
Mat dx( Vec &x, Mat &f, std::string method="spline") {
/*
  derivative with respect to SECOND index
*/

  size_t nx = f.size(1);
  if ( x.size() != nx ) {
    cerr << "dx ERROR sizes " << x.size() << " and " << nx <<
            " are incompatible \n";
    exit(1);
  }
  size_t ny = f.size(0);
  Mat d(ny, nx);

  switch (method[0]){

  case 'l':
    d = dxlag( x, f );
    break;

  default:

  Vec dx(nx);
  Vec fx(nx);
  Vec_int J(nx);
  J.range(0);
  for(size_t i=0; i<ny; ++i) {
    fx = f(i, J);
    cubspline fc(x, fx );
    dx = fc.prime( x );
    d.slip( dx, i, J);
    // for (size_t j=0; j<nx; ++j) d(i,j) = dx(j);
  }
  }
reg_near_axis( d, x, 4 );
return d;
}


#endif // __deriv2d__
