/**
 * $Id: czspline_capi.h,v 1.1 2008/05/23 18:30:48 Rob_Andre Exp $
 *
 *---------------------------------------------------------------------------
 * This code was developed at Tech-X (www.txcorp.com). It is free for any one
 * to use but comes with no warranty whatsoever. Use at your own risk. 
 * Thanks for reporting bugs to pletzer@txcorp.com. 
 *---------------------------------------------------------------------------
 * 
 * C callable prototypes for czspline methods
 */

#include "f77name.h"
#include "czspline_handle_size.h"
#include <stdlib.h>

#ifndef CZSPLINE_CAPI_H
#define CZSPLINE_CAPI_H

#ifdef __cplusplus
extern "C" {
#endif

/** 
 * Constructor. Allocate and set default values.
 * @param handle array of _ARRSZ integers
 * @param n1[, n2[, n3]] axis sizes
 * @param bcs1[, bcs2[, bcs3]] left/right boundary condition type 
 *        (-1=periodic, 0=not-a-knot, 1=1st derivative, 2=2nd derivative)
 *        this should be a 2-value array
 * @param ier error code (0=ok)       
 */
#define _S F77NAME(czspline_init1_r4)
  void _S(int handle[], const int *n1, 
	  const int bcs1[], 
	  int *ier);
#undef _S

#define _S F77NAME(czspline_init2_r4)
  void _S(int handle[], const int *n1, const int *n2, 
	  const int bcs1[], const int bcs2[], 
	  int *ier);
#undef _S

#define _S F77NAME(czspline_init3_r4)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const int bcs1[], const int bcs2[], const int bcs3[], 
	  int *ier);
#undef _S

#define _S F77NAME(czspline_init1_r8)
  void _S(int handle[], const int *n1, 
	  const int bcs1[], 
	  int *ier);
#undef _S

#define _S F77NAME(czspline_init2_r8)
  void _S(int handle[], const int *n1, const int *n2, 
	  const int bcs1[], const int bcs2[], 
	  int *ier);
#undef _S

#define _S F77NAME(czspline_init3_r8)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const int bcs1[], const int bcs2[], const int bcs3[], 
	  int *ier);
#undef _S

/** 
 * Destructor
 * @param handle array of _ARRSZ integers
 * @param ier error code (0=ok)       
 */
#define _S F77NAME(czspline_free1_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_free2_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_free3_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_free1_r8)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_free2_r8)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_free3_r8)
  void _S(int handle[], int *ier);
#undef _S

/** 
 * Set axes (by default axes go from (0,1) or (0, 2*pi) if periodic
 * @param handle array of _ARRSZ integers
 * @param n1[, n2[, n3]] axis sizes
 * @param x1[, x2[, x3]] axes
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_set_axes1_r4)
  void _S(int handle[], const int *n1, const _P x1[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_set_axes2_r4)
  void _S(int handle[], const int *n1, const int *n2, 
	  const _P x1[], const _P x2[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_set_axes3_r4)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const _P x1[], const _P x2[], const _P x3[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_axes1_r8)
  void _S(int handle[], const int *n1, const _P x1[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_axes2_r8)
  void _S(int handle[], const int *n1, const int *n2, 
	  const _P x1[], const _P x2[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_axes3_r8)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const _P x1[], const _P x2[], const _P x3[], int *ier);
#undef _P
#undef _S

/** 
 * Set flag for Akima interpolation (by default spline)
 * @param handle array of _ARRSZ integers
 * @param flag (1=Akima, 0=spline)
 * @param ier error code (0=ok)       
 */
#define _S F77NAME(czspline_set_ishermite1_r4)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

#define _S F77NAME(czspline_set_ishermite2_r4)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

#define _S F77NAME(czspline_set_ishermite3_r4)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

#define _S F77NAME(czspline_set_ishermite1_r8)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

#define _S F77NAME(czspline_set_ishermite2_r8)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

#define _S F77NAME(czspline_set_ishermite3_r8)
  void _S(int handle[], const int *flag, int *ier);
#undef _S

/** 
 * Set boundary condition values
 * @param handle array of _ARRSZ integers
 * @param bcval1[, bcval2[, bcval3]] 1st or 2nd derivative values 
 *         (depending of bc type). This should be a 2-element array.
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_set_bcvals1_r4)
  void _S(int handle[], const _P bcval1[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_set_bcvals2_r4)
  void _S(int handle[], const _P bcval1[], const _P bcval2[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_set_bcvals3_r4)
  void _S(int handle[], const _P bcval1[], const _P bcval2[], const _P bcval3[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_bcvals1_r8)
  void _S(int handle[], const _P bcval1[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_bcvals2_r8)
  void _S(int handle[], const _P bcval1[], const _P bcval2[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_set_bcvals3_r8)
  void _S(int handle[], const _P bcval1[], const _P bcval2[], const _P bcval3[], int *ier);
#undef _P
#undef _S

/** 
 * Compute spline or Hermite coefficients
 * @param handle array of _ARRSZ integers
 * @param n1[, n2[, n3]] axis sizes
 * @param f array of original data values of size n1*n2*n3 (contiguous in n1)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_setup1_r4)
  void _S(int handle[], const int *n1, const _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_setup2_r4)
  void _S(int handle[], const int *n1, const int *n2, const _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_setup3_r4)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_setup1_r8)
  void _S(int handle[], const int *n1, const _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_setup2_r8)
  void _S(int handle[], const int *n1, const int *n2, const _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_setup3_r8)
  void _S(int handle[], const int *n1, const int *n2, const int *n3, 
	  const _P f[], int *ier);
#undef _P
#undef _S

/** 
 * Single value (point) interpolation
 * @param handle array of _ARRSZ integers
 * @param p1[, p2[, p3]] target coordinates
 * @param f interpolated value
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_interp1_r4)
  void _S(int handle[], const _P *p1, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp2_r4)
  void _S(int handle[], const _P *p1, const _P *p2,
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp3_r4)
  void _S(int handle[], const _P *p1, const _P *p2, const _P *p3, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp1_r8)
  void _S(int handle[], const _P *p1, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp2_r8)
  void _S(int handle[], const _P *p1, const _P *p2,
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp3_r8)
  void _S(int handle[], const _P *p1, const _P *p2, const _P *p3, 
	  _P *f, int *ier);
#undef _P
#undef _S

/** 
 * Cloud interpolation
 * @param handle array of _ARRSZ integers
 * @param k number of points
 * @param p1[, p2[, p3]] target coordinates (each of size k)
 * @param f interpolated values (of size k)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_interp1_cloud_r4)
  void _S(int handle[], const int *k, const _P p1[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp2_cloud_r4)
  void _S(int handle[], const int *k, const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp3_cloud_r4)
  void _S(int handle[], const int *k, const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp1_cloud_r8)
  void _S(int handle[], const int *k, const _P p1[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp2_cloud_r8)
  void _S(int handle[], const int *k, const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp3_cloud_r8)
  void _S(int handle[], const int *k, const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

/** 
 * Array interpolation
 * @param handle array of _ARRSZ integers
 * @param k1[, k2[, k3]] number of points along each axis
 * @param p1[, p2[, p3]] target coordinates (of size resp. k1, k2, k3)
 * @param f interpolated values (of size k*k2*k3, contiguous in k1)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_interp1_array_r4)
  void _S(int handle[], const int *k1,  
	  const _P p1[],
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp2_array_r4)
  void _S(int handle[], const int *k1, const int *k2, 
	  const _P p1[], const _P p2[],
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_interp3_array_r4)
  void _S(int handle[], const int *k1, const int *k2, const int *k3, 
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp1_array_r8)
  void _S(int handle[], const int *k1,  
	  const _P p1[],
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp2_array_r8)
  void _S(int handle[], const int *k1, const int *k2, 
	  const _P p1[], const _P p2[],
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_interp3_array_r8)
  void _S(int handle[], const int *k1, const int *k2, const int *k3, 
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

/** 
 * Single point derivative computation
 * @param handle array of _ARRSZ integers
 * @param i1[, i2[, i3]] order of derivative along each axis 
 *                       max order depends on interpolation method
 *                       (0 <= i1, i2, i3 <= 2 typically)
 * @param p1[, p2[, p3]] target coordinate
 * @param f return value
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_derivative1_r4)
  void _S(int handle[],
	  const int *i1, 
	  const _P *p1,  
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative2_r4)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const _P *p1, const _P *p2, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative3_r4)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const _P *p1, const _P *p2, const _P *p3, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative1_r8)
  void _S(int handle[],
	  const int *i1, 
	  const _P *p1,  
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative2_r8)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const _P *p1, const _P *p2, 
	  _P *f, int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative3_r8)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const _P *p1, const _P *p2, const _P *p3, 
	  _P *f, int *ier);
#undef _P
#undef _S

/** 
 * Derivative computation on a cloud of points
 * @param handle array of _ARRSZ integers
 * @param i1[, i2[, i3]] order of derivative along each axis 
 *                       max order depends on interpolation method
 *                       (0 <= i1, i2, i3 <= 2 typically)
 * @param k number of points
 * @param p1[, p2[, p3]] target coordinates
 * @param f return values
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_derivative1_cloud_r4)
  void _S(int handle[],
	  const int *i1,
	  const int *k,
	  const _P p1[],  
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative2_cloud_r4)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const int *k,
	  const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative3_cloud_r4)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative1_cloud_r8)
  void _S(int handle[],
	  const int *i1, 
	  const int *k,
	  const _P p1[],  
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative2_cloud_r8)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const int *k,
	  const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative3_cloud_r8)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

/** 
 * Derivative computation on target rectilinear mesh
 * @param handle array of _ARRSZ integers
 * @param i1[, i2[, i3]] order of derivative along each axis 
 *                       max order depends on interpolation method
 *                       (0 <= i1, i2, i3 <= 2 typically)
 * @param k1[, k2[, k3] number of points along each axis
 * @param p1[, p2[, p3]] target coordinates
 * @param f array of size k1*k2*k3 of return values (contiguous in k1)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_derivative1_array_r4)
  void _S(int handle[],
	  const int *i1,
	  const int *k1,
	  const _P p1[],  
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative2_array_r4)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const int *k1, const int *k2, 
	  const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_derivative3_array_r4)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative1_array_r8)
  void _S(int handle[],
	  const int *i1, 
	  const int *k1,
	  const _P p1[],  
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative2_array_r8)
  void _S(int handle[],
	  const int *i1, const int *i2,
	  const int *k1, const int *k2,
	  const _P p1[], const _P p2[], 
	  _P f[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_derivative3_array_r8)
  void _S(int handle[],
	  const int *i1, const int *i2, const int *i3,
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P f[], int *ier);
#undef _P
#undef _S

/** 
 * Single point gradient computation
 * @param handle array of _ARRSZ integers
 * @param p1[, p2[, p3]] target coordinate
 * @param df ndim return values (df/dx, df/dy, ..)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_gradient1_r4)
  void _S(int handle[],
	  const _P *p1,  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient2_r4)
  void _S(int handle[],
	  const _P *p1, const _P *p2, 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient3_r4)
  void _S(int handle[],
	  const _P *p1, const _P *p2, const _P *p3, 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient1_r8)
  void _S(int handle[],
	  const _P *p1,  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient2_r8)
  void _S(int handle[],
	  const _P *p1, const _P *p2, 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient3_r8)
  void _S(int handle[],
	  const _P *p1, const _P *p2, const _P *p3, 
	  _P df[], int *ier);
#undef _P
#undef _S

/** 
 * Gradient computation on a cloud of points
 * @param handle array of _ARRSZ integers
 * @param k number of points
 * @param p1[, p2[, p3]] target coordinates
 * @param f k*ndim return values (contiguous in k)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_gradient1_cloud_r4)
  void _S(int handle[],
	  const int *k,
	  const _P p1[],  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient2_cloud_r4)
  void _S(int handle[],
	  const int *k,
	  const _P p1[], const _P p2[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient3_cloud_r4)
  void _S(int handle[],
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient1_cloud_r8)
  void _S(int handle[],
	  const int *k,
	  const _P p1[],  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient2_cloud_r8)
  void _S(int handle[],
	  const int *k,
	  const _P p1[], const _P p2[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient3_cloud_r8)
  void _S(int handle[],
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P df[], int *ier);
#undef _P
#undef _S

/** 
 * Gradient computation on target rectilinear mesh
 * @param handle array of _ARRSZ integers
 * @param k1[, k2[, k3] number of points along each axis
 * @param p1[, p2[, p3]] target coordinates
 * @param df array of size k1*k2*k3*ndim of return values (contiguous in k1)
 * @param ier error code (0=ok)       
 */
#define _P float
#define _S F77NAME(czspline_gradient1_array_r4)
  void _S(int handle[],
	  const int *k1,
	  const _P p1[],  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient2_array_r4)
  void _S(int handle[],
	  const int *k1, const int *k2, 
	  const _P p1[], const _P p2[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_gradient3_array_r4)
  void _S(int handle[],
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient1_array_r8)
  void _S(int handle[],
	  const int *k1,
	  const _P p1[],  
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient2_array_r8)
  void _S(int handle[],
	  const int *k1, const int *k2,
	  const _P p1[], const _P p2[], 
	  _P df[], int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_gradient3_array_r8)
  void _S(int handle[],
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  _P df[], int *ier);
#undef _P
#undef _S

/** 
 * Save object in file
 * @param handle array of _ARRSZ integers
 * @param filename 
 * @param ier error code (0=ok)
 * @param sz length of filename 
 */
#define _S F77NAME(czspline_save1_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_save2_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_save3_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_save1_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_save2_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_save3_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

/** 
 * Load object from file
 * @param handle array of _ARRSZ integers
 * @param filename 
 * @param ier error code (0=ok)
 * @param sz length of filename 
 */
#define _S F77NAME(czspline_load1_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_load2_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_load3_r4)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_load1_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_load2_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

#define _S F77NAME(czspline_load3_r8)
  void _S(int handle[], const char *filename, int *ier, size_t sz);
#undef _S

/** 
 * Test if grid is regular, ie axes are monotonically increasing
 * @param handle array of _ARRSZ integers
 * @param ier error code (0=ok)
 */
#define _S F77NAME(czspline_isgridregular1_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_isgridregular2_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_isgridregular3_r4)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_isgridregular1_r8)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_isgridregular2_r8)
  void _S(int handle[], int *ier);
#undef _S

#define _S F77NAME(czspline_isgridregular3_r8)
  void _S(int handle[], int *ier);
#undef _S

/** 
 * Test if point coordinate is in domain
 * @param handle array of _ARRSZ integers
 * @param p1[, p2[, p3]] coordinate
 * @param ier error code (0=ok)
 */
#define _P float
#define _S F77NAME(czspline_isindomain1_r4)
  void _S(int handle[], 
	  const _P *p1,
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain2_r4)
  void _S(int handle[], 
	  const _P *p1, const _P *p2,
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain3_r4)
  void _S(int handle[], 
	  const _P *p1, const _P *p2, const _P *p3, 
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain1_r8)
  void _S(int handle[], 
	  const _P *p1,
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain2_r8)
  void _S(int handle[], 
	  const _P *p1, const _P *p2,
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain3_r8)
  void _S(int handle[], 
	  const _P *p1, const _P *p2, const _P *p3, 
	  int *ier);
#undef _P
#undef _S

/** 
 * Test if all clouds point coordinates are in domain
 * @param handle array of _ARRSZ integers
 * @param k number of points
 * @param p1[, p2[, p3]] coordinates
 * @param ier error code (0=ok)
 */
#define _P float
#define _S F77NAME(czspline_isindomain1_cloud_r4)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[],
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain2_cloud_r4)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[], const _P p2[],
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain3_cloud_r4)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain1_cloud_r8)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[],
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain2_cloud_r8)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[], const _P p2[],
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain3_cloud_r8)
  void _S(int handle[], 
	  const int *k,
	  const _P p1[], const _P p2[], const _P p3[], 
	  int *ier);
#undef _P
#undef _S

/** 
 * Test if all array point coordinates are in domain
 * @param handle array of _ARRSZ integers
 * @param k1[, k2[, k3]] number of points along each axis
 * @param p1[, p2[, p3]] coordinates along each axis
 * @param ier error code (0=ok)
 */
#define _P float
#define _S F77NAME(czspline_isindomain1_array_r4)
  void _S(int handle[], 
	  const int *k1,
	  const _P p1[],
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain2_array_r4)
  void _S(int handle[], 
	  const int *k1, const int *k2,
	  const _P p1[], const _P p2[],
	  int *ier);
#undef _P
#undef _S

#define _P float
#define _S F77NAME(czspline_isindomain3_array_r4)
  void _S(int handle[], 
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain1_array_r8)
  void _S(int handle[], 
	  const int *k1,
	  const _P p1[],
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain2_array_r8)
  void _S(int handle[], 
	  const int *k1, const int *k2,
	  const _P p1[], const _P p2[],
	  int *ier);
#undef _P
#undef _S

#define _P double
#define _S F77NAME(czspline_isindomain3_array_r8)
  void _S(int handle[], 
	  const int *k1, const int *k2, const int *k3,
	  const _P p1[], const _P p2[], const _P p3[], 
	  int *ier);
#undef _P
#undef _S


#ifdef __cplusplus
}
#endif
  
#endif /* CZSPLINE_CAPI_H */
