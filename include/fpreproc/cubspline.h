/* -*-C++-*- */

#ifndef __cubspline__
#define __cubspline__

#ifdef NOBOOL
#define bool int
#define true 1
#define false 0
#endif /* NOBOOL */

#include "Vector.h"
#include <string>
#include <iostream>

// A Pletzer 1998, 1999, 2001
// I Szczesniak 2001

const Vec_int cubspline_itwo_zeros(2,0);
const Vec cubspline_two_zeros(2,0.0);

/** 1D Cubic spline class accomodating several boundary condition types
    including not-a-knot and periodic. Once cubic spline coefficients 
    are computed using either the constructor call or 'load' method, 
    interpolation as well as derivatives up to and including order three
    can be obtained either pointwise or on grids. Integration routines are
    also provided. 
    All vectors Vec are Vector<double> whereas Vec_int are Vector<size_t>.
 */

class cubspline {

private: 
  Vec b, c, d;

  /// number of nodes
  size_t nsize;

public: 
  /// vector of abscissae
  Vec x;
  /// vector of ordinates
  Vec y;

  /// true if spline coefficients have been initialized
  bool init;

  /// true if periodic
  bool periodic;

  /// A 2-integer vector defining the type of boundary conditions.
  Vec_int bc_type;
  /// Boundary condition values
  Vec bc_value;

  /// Min of abscissae
  double xmin;
  /// Max of abscissae
  double xmax;
  /// Min of ordinates
  double ymin;
  /// Max of ordinates
  double ymax;


  /*.............
    constructors 
    ............*/

  /// Constructor. 
  cubspline();
  
  /**
    Constructor. Build object given a set of (x, y) coordinates and some
    boundary condition constraints.

    @param x1 original abscissae
    @param y1 original ordinates
    @param bc_type1 Boundary condition types: 0 for not a knot, 
    1 for setting the derivative, 2 for imposing the second derivative.
    @param bc_value1 Values of the boundary conditions (not used for not-a-knot).
    */

  cubspline(const Vec &x1, const Vec &y1, Vec_int bc_type1=cubspline_itwo_zeros, Vec bc_value1=cubspline_two_zeros);

  /** Constructor for splines with periodic boundary conditions.
      @param x1 original abscissae
      @param y1 original ordinates
      @param per if ="periodic" then periodic boundary conditions
  */
  cubspline(const Vec &x1, const Vec &y1, std::string per);

  /** Build spline object. 
      @param x original abscissae
      @param y original ordinates
      @param bc_type1 Boundary condition types: 0 for not a knot, 
      1 for setting the derivative, 2 for imposing the second derivative.
      @see cubspline()
  */
  void load(const Vec &x, const Vec &y, Vec_int bc_type1, Vec bc_value1);

  /** Build spline object. Not-a-knot boundary conditions.
      @param x original abscissae
      @param y original ordinates
      @param bc_type1 Boundary condition types: 0 for not a knot, 
      1 for setting the derivative, 2 for imposing the second derivative.
      @see cubspline()
  */  
  void load(const Vec &x, const Vec &y);

  /** Build spline object. Periodic boundary conditions.
      @param x original abscissae
      @param y original ordinates
      @param bc_type1 Boundary condition types: 0 for not a knot, 
      1 for setting the derivative, 2 for imposing the second derivative.
      @see cubspline()
  */  
  void load(const Vec &x, const Vec &y, std::string per);  

  /** Check if xi < xmin. Look for nodes below interval.
      @param xi grid
      @return true if there is a node such that xi < xmin
  */
  bool is_below(Vec &xi);    

  /** Check if xi > xmax. Look for nodes above interval.
      @param xi grid
      @return true if there is a node such that xi > xmax
  */
   bool is_above(Vec &xi);    

  /** Check if xi < xmin and xi > xmax. Look for nodes outside interval.
      @param xi grid
      @return true if there is a node such that xi > xmax and xi < xmin
  */
  bool is_extra(Vec &xi);

  /** Return 1's for all nodes xi that are inside [min(x), max(x)].
      @param xi grid
      @return vector 0's for nodes outside [xmin, xmax], resp 1's for nodes inside.
  */
  Vec mask(const Vec &xi);

  /* Return 1 if node is inside [min(x), max(x)]
     @param abscissa
     @return result = 1 or 0
  */
  double mask(const double xi);

  /** Return indices of nodes closest to the left of xi.
      @param xi abscissae
      @return result = node indices
  */
  Vec_int bra(const Vec &xi); 

  /** Return index of node closest to the left of xi.
      @param xi abscissa
      @return result = node index
  */
  size_t bra(const double xi);

  /** Return indices of nodes nearest to xi.
      @param xi abscissae
      @return result = node indices
  */
  Vec_int near(const Vec &xi);

  /** Return index of node nearest xi.
      @param xi abscissa
      @return result = node index
  */
  size_t near(const double xi);

 /** Return indices of nodes closest to the right of xi.
      @param xi abscissae
      @return result = node indices
  */
  Vec_int ket(const Vec &xi); 

  /** Return index of node closest to the right of xi.
      @param xi abscissa
      @return result = node index
  */
  size_t ket(const double xi); 

 /** Step function interpolation. 
      @param xi abscissae
      @return result = piecewise constant interpolation.
  */
  Vec step(const Vec &xi);   
 
 /** Step function interpolation. 
      @param xi abscissa
      @return result = piecewise constant interpolation.
  */
  double step(const double xi);

 /** Linear function interpolation. 
      @param xi abscissae
      @return result = piecewise linear interpolation.
  */
  Vec linear(const Vec &xi);

 /** Linear function interpolation. 
      @param xi abscissa
      @return result = piecewise linear interpolation.
  */
  double linear(const double xi);

 /** Cubic function interpolation. 
      @param xi abscissae
      @return result = piecewise linear interpolation.
  */
  Vec cubic(const Vec &xi);

 /** Cubic function interpolation. 
      @param xi abscissa
      @return result = piecewise linear interpolation.
  */
  double cubic(const double xi);

  /** First derivative.  
      @param xi  abscissae
      @param method can be "cubic" or "linear"
      @return result = df/dx
  */
  Vec prime(const Vec &xi, std::string method="cubic");

  /** First derivative.  
      @param xi  abscissa
      @param method can be "cubic" or "linear"
      @return result = df/dx
  */
  double prime(const double xi, std::string method="cubic");

  /** Second derivative.  
      @param xi  abscissae
      @return result = d2f/dx2
  */
  Vec second(const Vec &xi);  

  /** Second derivative.  
      @param xi  abscissa
      @return result = d2f/dx2
  */
  double second(const double xi);

  /** Third derivative.  
      @param xi  abscissae
      @return result = d3f/dx3
  */
  Vec third(const Vec &xi);   

  /** Third derivative.  
      @param xi  abscissa
      @return result = d3f/dx3
  */
  double third(const double xi);

  /** Integrate from xmin to xi_to.
      @param xi_to right end point (array)
      @return integral
  */
  Vec integrate(const Vec &xi_to);

  /** Integrate from xmin to xi_to.
      @param xi_to right end point
      @return integral
  */
  double integrate(const double xi_to);
};

#endif /* __cubspline__ */
