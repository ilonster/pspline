/**
 * $Id: czspline_test.cxx,v 1.1 2008/05/23 18:32:09 Rob_Andre Exp $
 *
 *---------------------------------------------------------------------------
 * This code was developed at Tech-X (www.txcorp.com). It is free for any one
 * to use but comes with no warranty whatsoever. Use at your own risk. 
 * Thanks for reporting bugs to pletzer@txcorp.com. 
 *---------------------------------------------------------------------------
 * 
 * Test C++ bindings to ezspline
 */
#include "czspline_capi.h"

// std includes
#include <iostream>
#include <vector>
#include <cmath>
#include <cassert>

/** 
 * Test 1d interpolation (precision = r8)
 */
void test1d_r8() {

  // opaque handle
  int handle[_ARRSZ];
 
  // error code
  int ier = 0;
  // boundary condition type
  int bcs1[2]; 
  // not-a-knot
  bcs1[0] = 0; bcs1[1] = 0;
  
  // create axes
  int n1 = 11;
  double dx = 1.0 / double(n1 - 1);
  std::vector<double> x1(n1);
  for (int i = 0; i < n1; ++i) 
    x1[i] = i * dx;
  // constructor
  F77NAME(czspline_init1_r8)(handle, &n1, bcs1, &ier);
  assert(ier == 0);
  F77NAME(czspline_set_axes1_r8)(handle, &n1, &x1[0], &ier);
  assert(ier == 0);

  // set original data
  std::vector<double> f(n1); 
  for (int i = 0; i < n1; ++i) 
    f[i] = x1[i]*x1[i]*x1[i];
  F77NAME(czspline_setup1_r8)(handle, &n1, &f[0], &ier);
  assert(ier == 0);
  
  // target axes
  int m1 = 101;
  double dy = 1.0 / double(m1 - 1);
  std::vector<double> y1(m1);
  for (int i = 0; i < m1; ++i) 
    y1[i] = i * dy;

  // interpolated values
  std::vector<double> g(m1);
  
  // point interpolation
  int m = m1/2;
  double y = y1[m];
  F77NAME(czspline_interp1_r8)(handle, &y, &g[m], &ier);
  assert(ier == 0);
  double error_point = std::abs(g[m] - y*y*y);

  // cloud interpolation
  F77NAME(czspline_interp1_cloud_r8)(handle, &m1, &y1[0], &g[0], &ier);
  assert(ier == 0);
  double error_cloud = 0.0;
  for (int i = 0; i < m1; ++i)
    error_cloud += std::abs(g[i] - y1[i]*y1[i]*y1[i]);
  error_cloud /= double(m1);

  // array interpolation
  F77NAME(czspline_interp1_array_r8)(handle, &m1, &y1[0], &g[0], &ier);
  assert(ier == 0);
  double error_array = 0.0;
  for (int i = 0; i < m1; ++i)
    error_array += std::abs(g[i] - y1[i]*y1[i]*y1[i]);
  error_array /= double(m1);

  // save to file
  std::string fname = "test1d_r8.nc";
  F77NAME(czspline_save1_r8)(handle, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // load from file
  int handle2[_ARRSZ];
  F77NAME(czspline_load1_r8)(handle2, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // clean up
  F77NAME(czspline_free1_r8)(handle, &ier);
  assert(ier == 0);
  F77NAME(czspline_free1_r8)(handle2, &ier);
  assert(ier == 0);

  std::cout << "test1d_r8 interpolation errors\n";
  std::cout << "point: " << error_point << '\n';
  std::cout << "cloud: " << error_cloud << '\n';
  std::cout << "array: " << error_array << '\n';
}

/** 
 * Test 2d interpolation (precision = r4)
 */
void test2d_r4() {

  // opaque handle
  int handle[_ARRSZ];

  // error code
  int ier = 0;
  // boundary condition type
  int bcs1[2]; 
  // not-a-knot
  bcs1[0] = 0; bcs1[1] = 0;
  int bcs2[2]; 
  // periodic
  bcs2[0] = -1; bcs2[1] = -1;
  
  // create axes
  int n1 = 11;
  int n2 = 12;
  float pi = 3.1415926535897931f;
  float dx1 = 1.0 / float(n1 - 1);
  float dx2 = 2.0f * pi / float(n2 - 1);
  std::vector<float> x1(n1);
  for (int i = 0; i < n1; ++i) 
    x1[i] = i * dx1;
  std::vector<float> x2(n2);
  for (int i = 0; i < n2; ++i) 
    x2[i] = i * dx2;
  // constructor
  F77NAME(czspline_init2_r4)(handle, &n1, &n2, bcs1, bcs2, &ier);
  assert(ier == 0);
  F77NAME(czspline_set_axes2_r4)(handle, &n1, &n2, &x1[0], &x2[0], &ier);
  assert(ier == 0);

  // set original data
  std::vector<float> f(n1*n2);
  for (int i2 = 0; i2 < n2; ++i2) {
    for (int i1 = 0; i1 < n1; ++i1) {
      int i = i1 + n1 * i2;
      f[i] = x1[i1]*x1[i1]*x1[i1] * cos(x2[i2]);
    }
  }
  F77NAME(czspline_setup2_r4)(handle, &n1, &n2, &f[0], &ier);
  assert(ier == 0);
  
  // target axes
  int m1 = 101;
  int m2 = 102;
  float dy1 = 1.0 / float(m1 - 1);
  float dy2 = 2.0f * pi / float(m2 - 1);
  std::vector<float> y1(m1);
  for (int i = 0; i < m1; ++i) 
    y1[i] = i * dy1;
  std::vector<float> y2(m2);
  for (int i = 0; i < m2; ++i) 
    y2[i] = i * dy2;

  // interpolated values
  std::vector<float> g(m1 * m2);
  
  // point interpolation
  int i1p = m1/2;
  int i2p = m2/2;
  float y1p = y1[i1p];
  float y2p = y2[i2p];
  int i = i1p + m1 * i2p;
  F77NAME(czspline_interp2_r4)(handle, &y1p, &y2p, &g[i], &ier);
  assert(ier == 0);
  float error_point = std::abs(g[i] - y1p*y1p*y1p * cos(y2p));

  // cloud interpolation (m1 points)
  i1p = 0;
  i = i1p + i2p * m1;
  std::vector<float> y2pp(m1, y2p);
  F77NAME(czspline_interp2_cloud_r4)(handle, &m1, &y1[i1p], &y2pp[0], 
				     &g[i], &ier);
  assert(ier == 0);
  float error_cloud = 0.0f;
  for (int i1 = 0; i1 < m1; ++i1)
    error_cloud += std::abs(g[i + i1] - y1[i1]*y1[i1]*y1[i1]*cos(y2[i2p]));
  error_cloud /= float(m1);

  // array interpolation
  F77NAME(czspline_interp2_array_r4)(handle, &m1, &m2, 
				     &y1[0], &y2[0], &g[0], &ier);
  assert(ier == 0);
  float error_array = 0.0;
  for (int i2 = 0; i2 < m2; ++i2) {
    for (int i1 = 0; i1 < m1; ++i1) {
      int i = i1 + i2 * m1;
      error_array += std::abs(g[i] - y1[i1]*y1[i1]*y1[i1] * cos(y2[i2]));
    }
  }
  error_array /= float(m1 * m2);

  // save to file
  std::string fname = "test2d_r4.nc";
  F77NAME(czspline_save2_r4)(handle, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // load from file
  int handle2[_ARRSZ];
  F77NAME(czspline_load2_r4)(handle2, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // clean up
  F77NAME(czspline_free2_r4)(handle, &ier);
  assert(ier == 0);
  F77NAME(czspline_free2_r4)(handle2, &ier);
  assert(ier == 0);

  std::cout << "test2d_r4 interpolation errors\n";
  std::cout << "point: " << error_point << '\n';
  std::cout << "cloud: " << error_cloud << '\n';
  std::cout << "array: " << error_array << '\n';
}

/** 
 * Test 3d interpolation (precision = r8)
 */
void test3d_r8() {

  // opaque handle
  int handle[_ARRSZ];

  // error code
  int ier = 0;
  // boundary condition type
  int bcs1[2]; 
  // not-a-knot
  bcs1[0] = 0; bcs1[1] = 0;
  int bcs2[2]; 
  // periodic
  bcs2[0] = -1; bcs2[1] = -1;
  int bcs3[2];
  // slope and 2nd derivative
  bcs3[0] = +1; bcs3[1] = +2;

  // create axes
  int n1 = 11;
  int n2 = 12;
  int n3 = 13;
  double pi = 3.1415926535897931;
  double dx1 = 1.0 / double(n1 - 1);
  double dx2 = 2.0 * pi / double(n2 - 1);
  double dx3 = 2.0 * pi / double(n3 - 1);
  std::vector<double> x1(n1);
  for (int i = 0; i < n1; ++i) 
    x1[i] = i * dx1;
  std::vector<double> x2(n2);
  for (int i = 0; i < n2; ++i) 
    x2[i] = i * dx2;
  std::vector<double> x3(n3);
  for (int i = 0; i < n3; ++i) 
    x3[i] = i * dx3;
  // constructor
  F77NAME(czspline_init3_r8)(handle, &n1, &n2, &n3, 
			     bcs1, bcs2, bcs3, &ier);
  assert(ier == 0);
  // boundary conditions (values for periodic and not-a-knot
  // are not used but must still be passed to the setter)
  double bcval1[2];
  double bcval2[2];
  double bcval3[2];
  bcval3[0] = 1.0; // df/dx3 @ x3=0
  bcval3[1] = 0.0; // d^2/dx3^2 @ x3=2*pi
  F77NAME(czspline_set_bcvals3_r8)(handle, bcval1, bcval2, bcval3, &ier);
  assert(ier == 0);
  // set axes (necessary unless (0,..1) for anything but periodic BCs
  // or (0..2*pi) for periodic BCs
  F77NAME(czspline_set_axes3_r8)(handle, &n1, &n2, &n3, 
				 &x1[0], &x2[0], &x3[0], &ier);
  assert(ier == 0);

  // set original data
  std::vector<double> f(n1*n2*n3);
  for (int i3 = 0; i3 < n3; ++i3) {
    for (int i2 = 0; i2 < n2; ++i2) {
      for (int i1 = 0; i1 < n1; ++i1) {
	int i = i1 + n1*(i2 + n2*i3);
	f[i] = x1[i1]*x1[i1]*x1[i1] * cos(x2[i2]) * sin(x3[i3]);
      }
    }
  }
  F77NAME(czspline_setup3_r8)(handle, &n1, &n2, &n3, 
			      &f[0], &ier);
  assert(ier == 0);
  
  // target axes
  int m1 = 101;
  int m2 = 102;
  int m3 = 103;
  double dy1 = 1.0 / double(m1 - 1);
  double dy2 = 2.0 * pi / double(m2 - 1);
  double dy3 = 2.0 * pi / double(m3 - 1);
  std::vector<double> y1(m1);
  for (int i = 0; i < m1; ++i) 
    y1[i] = i * dy1;
  std::vector<double> y2(m2);
  for (int i = 0; i < m2; ++i) 
    y2[i] = i * dy2;
  std::vector<double> y3(m3);
  for (int i = 0; i < m3; ++i) 
    y3[i] = i * dy3;

  // interpolated values
  std::vector<double> g(m1 * m2 * m3);
  
  // point interpolation
  int i1p = m1/2;
  int i2p = m2/2;
  int i3p = m3/2;
  double y1p = y1[i1p];
  double y2p = y2[i2p];
  double y3p = y3[i3p];
  int i = i1p + m1*(i2p + m2*i3p);
  F77NAME(czspline_interp3_r8)(handle, &y1p, &y2p, &y3p, 
			       &g[i], &ier);
  assert(ier == 0);
  double error_point = std::abs(g[i] - y1p*y1p*y1p * cos(y2p) * sin(y3p));

  // cloud interpolation (m1 points along x1 axis)
  i1p = 0;
  i = i1p + m1*(i2p + m2*i3p);
  std::vector<double> y2pp(m1, y2p);
  std::vector<double> y3pp(m1, y3p);
  F77NAME(czspline_interp3_cloud_r8)(handle, &m1, 
				     &y1[i1p], &y2pp[0], &y3pp[0],
				     &g[i], &ier);
  assert(ier == 0);
  double error_cloud = 0.0;
  for (int i1 = 0; i1 < m1; ++i1)
    error_cloud += std::abs(g[i + i1] - 
			    y1[i1]*y1[i1]*y1[i1]*cos(y2[i2p])*sin(y3[i3p]));
  error_cloud /= double(m1);

  // array interpolation
  F77NAME(czspline_interp3_array_r8)(handle, &m1, &m2, &m3,
				     &y1[0], &y2[0], &y3[0],
				     &g[0], &ier);
  assert(ier == 0);
  double error_array = 0.0;
  for (int i3 = 0; i3 < m3; ++i3) {
    for (int i2 = 0; i2 < m2; ++i2) {
      for (int i1 = 0; i1 < m1; ++i1) {
	int i = i1 + m1*(i2 + m2*i3);
	error_array += std::abs(g[i] - 
				y1[i1]*y1[i1]*y1[i1] * cos(y2[i2]) * sin(y3[i3]));
      }
    }
  }
  error_array /= double(m1 * m2 * m3);

  // save to file
  std::string fname = "test3d_r8.nc";
  F77NAME(czspline_save3_r8)(handle, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // load from file
  int handle2[_ARRSZ];
  F77NAME(czspline_load3_r8)(handle2, fname.c_str(), &ier, fname.size());
  assert(ier == 0);

  // clean up
  F77NAME(czspline_free3_r8)(handle, &ier);
  assert(ier == 0);
  F77NAME(czspline_free3_r8)(handle2, &ier);
  assert(ier == 0);

  std::cout << "test3d_r8 interpolation errors\n";
  std::cout << "point: " << error_point << '\n';
  std::cout << "cloud: " << error_cloud << '\n';
  std::cout << "array: " << error_array << '\n';
}

/**
 * Main driver
 */
int main() {
    test1d_r8();
    test2d_r4();
    test3d_r8();
    return 0;
}
