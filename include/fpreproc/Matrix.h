/* -*-C++-*- */

// Matrix class
// - created by A. Pletzer, Aug 98,
// - additions and changes by Irek Szczesniak, Aug 2001

#ifndef _MATRIX_H_
#define _MATRIX_H_

#ifndef NO_ASSERT
#include <assert.h>
#endif

#include <math.h>
#include <stdlib.h>
#include <vector>
#include "Vector.h"


using namespace std;

/** Column oriented matrix class.
  
  Matrices are column majored, ie
  elements in a column are adjacent in memory.  This
  implementation can be used with Lapack and is compatible with the
  Fortran array layout, but not with the "C/C++" array layout.
  
  */

template <class T>
class ColMat
{

private:
  
   /// Stores the number of rows.
  size_t i_size;
  
  /// Stores the number of columns.
  size_t j_size;
  
public:

  /// Stores the matrix elements.
  Vector<T> v_;

 /** A constructor with no arguments.  
    To later use an object created with this constructor, a method
    "newsize" or "alloc" must be called to set the size. */
  ColMat();                             
  
  /** Create matrix. Elements are not set.
      @param i no. of rows 
      @param j no. of columns. */
  ColMat(size_t i, size_t j);

  /** Create matrix. Elelemnts are set.
    @param i the number of rows
    @param j the number of columns
    @param e value of each element
    */
  ColMat(size_t i, size_t j, const T& e);

  /// A copy constructor.
  ColMat(const ColMat<T>&); 

  /** Negation operator (elementwise).  */
  ColMat<T> operator-();

  /** Add "f" to every matrix
      element. */
  
  ColMat<T> operator+=(const T f);

  /** Subtract "f" from each matrix
      element. */
  ColMat<T> operator-=(const T f);

  /** Multiply each matrix element by the value
      of "f". */
  ColMat<T> operator*=(const T f);

  /** Divide each matrix element by "f". */
  ColMat<T> operator/=(const T f);

  /** Add another matrix "b". */
  ColMat<T> operator+=(const ColMat<T> &b);

  /** Subtract another matrix "b" from itself. */
  ColMat<T> operator-=(const ColMat<T> &b);

  /** Multiply each matrix element (of the matrix on
    the left of "*=") by the corresponding element of the "b" matrix*/
  ColMat<T> operator*=(const ColMat<T> &b);

  /** Divide each matrix element (of the matrix on
    the left of "*=") by the corresponding element of the "b" matrix.*/
  ColMat<T> operator/=(const ColMat<T> &b);

  /** The function that fills in the matrix with random elements
    between 0 and 1. Two subsequent calls to random will generate different
  elements. However, a program will consistently generate the same numbers. */
  void random(void);

  /** Indexing operator. 
    @param i the row number
    @param j the column number
    @return reference to an element pointed by i and j
   */
  inline T& operator()(size_t i, size_t j)
  {
#ifndef NO_ASSERT
    assert(("T& ColMat::operator()(size_t i, size_t j): " \
	    "index i out of range", i < i_size));

    assert(("T& ColMat::operator()(size_t i, size_t j): " \
	    "index j out of range", j < j_size));
#endif 
    
    return v_(j * i_size + i);
  }

  /** Indexing operator. 
    @param i the row number
    @param j the column number
    @return reference to an element pointed by i and j
   */
  inline const T& operator()(size_t i, size_t j) const
  {
#ifndef NO_ASSERT
    assert(("const T& ColMat::operator()(size_t i, size_t j): " \
	    "index i out of range", 
	    i < i_size));

    assert(("const T& ColMat::operator()(size_t i, size_t j): " \
	    "index j out of range",
	    j < j_size));
#endif 

    return v_(j * i_size + i);
  }

  /** Assignment operator. Set all elements. */
  ColMat<T> & operator=(const T&);

  /** Array slicing. Given 2 integer vectors I and J.
   */
  ColMat<T> operator()(const Vector<size_t> &I, const Vector<size_t> &J ) const;
  /** Array slicing. Given the integer vector I and a index j.
   */
  Vector<T> operator()(const Vector<size_t> &I, const size_t j ) const;
  /** Array slicing. Given index i and the integer vector J.
   */
  Vector<T> operator()(const size_t i, const Vector<size_t> &J ) const;

  /** Get Matrix sizes. The argument should be either 1 or 0
      @param i =0 to get the number of columns, =0 to get the number of rows.
      @return size
   */
  size_t size(size_t i) const; 
  
  /** Get the number of rows. */
  size_t isize() const;

  /** Get the number of columns. */
  size_t jsize() const;

  /** Resize the matrix. */
  ColMat<T>& newsize(size_t, size_t);

  /** Allocate space. Elements are not initialized. */
  ColMat<T>& alloc(const size_t nr, const size_t nc);

  
  ColMat<T> slip(const Vector<T> &v, const Vector<size_t> &I, const Vector<size_t> &J);
  ColMat<T> slip(const Vector<T> &v, const Vector<size_t> &I, const size_t j);
  ColMat<T> slip(const Vector<T> &v, const size_t i, const Vector<size_t> &J);
};                                                              

typedef ColMat<double> Mat;

/**@name Matrix Global Functions
  These are global functions operating on or generating matrices.  */

//@{

/** Average along rows.
  @param matrix
  @return vector of averages
 */
template<class T>
Vector<T> average0(const ColMat<T> &a);

/** Average along columns.
  @param matrix
  @return vector of averages
 */
template<class T>
Vector<T> average1(const ColMat<T> &a);

/** Sum along rows. 
  @param matrix
  @return vector of sums
  */
template<class T>
Vector<T> sum0(const ColMat<T> &a);

/** Sum along columns. 
  @param matrix
  @return vector of sums
  */
template<class T>
Vector<T> sum1(const ColMat<T> &a);

/** Matrix-matrix multiplication. Not to be confused with a*b, the elementwise
 multiplication. 
@see operator*
@param a a matrix
@param b another matrix of shape b.size(0)=a.size(1)
@return matrix = a.b
*/
template<class T> 
ColMat<T> MatMult(const ColMat<T> &a, const ColMat<T> &b);

/** Matrix-vector multiplication. 
 @param a a matrix
 @param b a vector of length b.size()=a.size(1) 
 @return vector = a.b
*/
template<class T> 
Vector<T> MatMult(const ColMat<T> &a, const Vector<T> &b);

/** Vector-matrix multiplication. 
 @param b a vector
 @param a a matrix of shape a.size(0) = b.size()
 @return vector = $b^T.a$
*/
template<class T> 
Vector<T> MatMult(const Vector<T> &b, const ColMat<T> &a);

/** Perform matrix-matrix-matrix multiplication. 
    More efficient than a nested matrix-matrix multiplication (MatMult),
    since it saves the creation of a temporary
    matrix.
@param a a matrix
@param b a matrix of shape b.size(0)=a.size(1)
@param c a matrix of shape c.size(0)=b.size(1)
@return matrix = a.b.c
 */
template<class T> 
ColMat<T> MatMult(const ColMat<T> &a, const ColMat<T> &b, const ColMat<T> &c);

/** Elementwise multiplication. Not to be confused with MatMult, which is the 
    matrix multiplication. 
    @see MatMult
    @param a a matrix
    @param b another matrix of shapre identical to a
    @return matrix =a*b (!= a.b)
*/
template<class T> 
ColMat<T> operator*(const ColMat<T> &a, const ColMat<T> &b);
     
/** Elementwise division by a scalar.
    @param a a matrix
    @param f a scalar
    @return matrix a/f
 */
template<class T> 
ColMat<T> operator/(const ColMat<T> &a, const T f);

/** Division  of a matrix of size equal to a filled with 'f' values, 
    by each element of a. This is equivalent to creating a matrix b=f 
    of size a, and dividing each element of b by a.
    @param f a scalar
    @param a a matrix
    @resturn matrix = f/a
 */
template<class T> 
ColMat<T> operator/(const T f, const ColMat<T> &a);

/** Elementwise division. Not to be confused with Inverse.
    @see Inverse
    @param a a matrix
    @param b a matrix of shape identical to a
    @return matrix a/b
 */
template<class T>
ColMat<T> operator/(const ColMat<T> &a, const ColMat<T> &b);

/** Apply function sin to each element.

  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> sin(const ColMat<T> &a);

/** Apply function cos to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> cos(const ColMat<T> &a);

/** Apply function tan to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> tan(const ColMat<T> &a);

/** Apply function asin to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> asin(const ColMat<T> &a);

/** Apply function acos to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> acos(const ColMat<T> &a);

/** Apply function atan to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> atan(const ColMat<T> &a);

/** Apply function exp to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> exp(const ColMat<T> &a);

/** Apply function log to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> log(const ColMat<T> &a);

/** Apply function sqrt to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> sqrt(const ColMat<T> &a);

/** Apply function abs to each element.
  @param a - input matrix
  @return matrix
*/
template<class T>
ColMat<T> abs(const ColMat<T> &a);

/** Apply function pow to each element.
  @param a - input matrix
  @param exp - exponent
  @return matrix
*/
template<class T>
ColMat<T> pow(const ColMat<T> &a, const T exp);

/** Apply function pow to each element.
  @param a - input matrix
  @param exp - integer exponent
  @return matrix
*/
template<class T>
ColMat<T> pow(const ColMat<T> &a, const int exp);


//  template<class T> 
//  ColMat<T> space1(T xmin, T xmax, size_t ny=1, size_t nx=11);

//  template<class T> 
//  ColMat<T> space0(T ymin, T ymax, size_t ny=11, size_t nx=1);

/** Stretch vertically, along rows. This is equivalent to duplicating 
 n0 times a row vector so as to create a matrix of size n0 times v.size() 
@param v input vector
@param n0 vertical size (or no. of rows)
@return matrix */
template<class T> 
ColMat<T> stretch0(const  Vector<T> &v, const size_t n0);

/** Stretch horizontally, along columns. This is equivalent to duplicating 
 n1 times a column vector so as to create a matrix of size v.size() times n1
@param v input vector
@param n1 horizontal size (or no. of columns)
@return matrix */
template<class T>
ColMat<T> stretch1(const Vector<T> &v, const size_t n1);

/* tensor product of 2 vectors = matrix */

//  template<class TYPE>
//  ColMat<TYPE> TensorProd(const Vector<TYPE> &a, const Vector<TYPE> &b);

/* convert matrix to vector by flattenning along columns or rows */

//  template<class TYPE>
//  Vector<TYPE> flatten0(const ColMat<TYPE> &a);

//  template<class TYPE>
//  Vector<TYPE> flatten1(const ColMat<TYPE> &a);

/** Get the minimum value. 
    @param a a matrix
    @return scalar = min(a)
 */
template<class T>
T min(const ColMat<T> &a);

/** Get the maximum value. 
    @param a a matrix
    @return scalar = max(a)
 */
template<class T>
T max(const ColMat<T> &a);

/** Comparative minimum.
    @param a a matrix
    @param b a scalar
    @return matrix filled with elements of 'a' when 'a' < 'b', 
    or 'b' otherwise.
 */
template<class T>
ColMat<T> min(const ColMat<T> &a, const T b);

/** Comparative maximum.
    @param a a matrix
    @param b a scalar
    @return matrix filled with elements of 'a' when 'a' > 'b', 
    or b_{i,j} otherwise.
 */
template<class T>
ColMat<T> max(const ColMat<T> &a, const T b);

/** Comparative minimum.
    @param a a matrix
    @param b another matrix of shape equal to a
    @return matrix filled with elements of 'a' when 'a' < 'b', or 'b' otherwise.
 */
template<class T>
ColMat<T> min(const ColMat<T> &a, const ColMat<T> &b);

/** Comparative maximum.
    @param a a matrix
    @param b another matrix of shape equal to a
    @return matrix filled with elements of 'a' when 'a' > 'b', or 'b' otherwise.
 */
template<class T>
ColMat<T> max(const ColMat<T> &a, const ColMat<T> &b);

//@}

// ***************************************************************

template <class T>
ColMat<T>::ColMat() : i_size(0), j_size(0)
{
}

template <class T>
ColMat<T>::ColMat(size_t m, size_t n) : v_(m*n), i_size(m), j_size(n)
{
}

template <class T>
ColMat<T>::ColMat(size_t m, size_t n, const T &elem)
  : v_(m*n, elem), i_size(m), j_size(n) 
{
}

template <class T>
ColMat<T>::ColMat(const ColMat<T> &s)
{
  operator=(s);
}

template <class T>
ColMat<T> &ColMat<T>::operator=(const T &f)
{
  transform(v_.begin(), v_.end(), v_.begin(), Setval<T>(f));
}


// return a pseudo-random matrix with elements between 0. and 1.
template<class T> 
void ColMat<T>::random(void) 
{
  transform(v_.begin(), v_.end(), v_.begin(), Random<T>());
}

template<class T> 
ColMat<T> ColMat<T>::operator+=(const T f)
{
  transform(v_.begin(), v_.end(), v_.begin(), bind2nd(plus<T>(), f));
  return *this;
}

template<class T>
ColMat<T> ColMat<T>::operator+=(const ColMat<T> &a) 
{
#ifndef NO_ASSERT
  assert(("ColMat<T> ColMat<T>::operator+=(const ColMat<T> &a): " \
	  "different sizes of matrixes", 
	  isize() == a.isize() && jsize() == a.jsize()));
#endif

  transform(v_.begin(), v_.end(), a.v_.begin(), v_.begin(), plus<T>());
  return *this;
}
     
template<class T> 
ColMat<T> operator+(const ColMat<T> &a, const T f)
{
  ColMat<T> c(a);
  return c+=f;
}

template<class T> 
ColMat<T> operator+(const T f, const ColMat<T> &a)
{
  ColMat<T> c(a);
  return c+=f;
}

template<class T> 
ColMat<T> operator+(const ColMat<T> &a, const ColMat<T> &b)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> operator+(const ColMat<T> &a, const ColMat<T> &b): " \
	  "different sizes of matrixes", 
	  a.isize() == b.isize() && a.jsize() == b.jsize()));
#endif

  ColMat<T> c(a);
  return c+=b;
}
     
template<class T> 
ColMat<T> ColMat<T>::operator-=(const T f)
{
  
  transform(v_.begin(), v_.end(), v_.begin(), bind2nd(minus<T>(), f));
  return *this;
}

template<class T> 
ColMat<T> ColMat<T>::operator-=(const ColMat<T> &a)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> ColMat<T>::operator-=(const ColMat<T> &a): " \
	  "different sizes of matrixes", 
	  isize() == a.isize() && jsize() == a.jsize()));
#endif

  transform(v_.begin(), v_.end(), a.v_.begin(), v_.begin(), minus<T>());
  return *this;
}

template<class T>
ColMat<T> ColMat<T>::operator-()
{
  ColMat<T> c(isize(), jsize());
  transform(v_.begin(), v_.end(), c.v_.begin(), negate<T>());
  return c;
}

template<class T> 
ColMat<T> operator-(const ColMat<T> &a, const T f)
{
  ColMat<T> c(a);
  return c-=f;
}

template<class T> 
ColMat<T> operator-(const T f, const ColMat<T> &a)
{
  ColMat<T> c(a.size(0), a.size(1), f);
  return c-=a;
}

template<class T> 
ColMat<T> operator-(const ColMat<T> &a, const ColMat<T> &b)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> operator-(const ColMat<T> &a, const ColMat<T> &b): " \
	  "different sizes of matrixes",
	  a.isize() == b.isize() && a.jsize() == b.jsize()));
#endif

  ColMat<T> c(a);
  c-=b;

  return c;
}

template<class T> 
ColMat<T> ColMat<T>::operator*=(const T f)
{
  transform(v_.begin(), v_.end(), v_.begin(), bind2nd(multiplies<T>(), f));
  return *this;
}

template<class T> 
ColMat<T> ColMat<T>::operator*=(const ColMat<T> &a)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> ColMat<T>::operator*=(const ColMat<T> &a): " \
	  "different sizes of matrixes", 
	  isize() == a.isize() && jsize() == a.jsize()));
#endif

  transform(v_.begin(), v_.end(), a.v_.begin(), v_.begin(), multiplies<T>());
  return *this;
}

template<class T> 
ColMat<T> operator*(const ColMat<T> &a, const T f)
{
  ColMat<T> c(a);
  return c*=f;
}

template<class T> 
ColMat<T> operator*(const T f, const ColMat<T> &a)
{
  ColMat<T> c(a);
  return c*=f;
}

template <class T>
std::ostream&   operator<<(std::ostream& s, const ColMat<T>& V)
{
  size_t M = V.size(0);
  size_t N = V.size(1);

  for (size_t i=0; i<M; i++)
    {
      for (size_t j=0; j<N; j++)
	s << V(i,j) << " " ;
      s << std::endl;
    }
    
  return s;
}

template<class T>
size_t ColMat<T>::isize() const
{
  return i_size;
}

template<class T>
size_t ColMat<T>::jsize() const
{
  return j_size;
}

template <class T>
ColMat<T> &ColMat<T>::alloc(const size_t ni_size, const size_t nj_size)
{
  v_.alloc(ni_size * nj_size);
  i_size = ni_size;
  j_size = nj_size;
  return *this;
}

template <class T>
ColMat<T>& ColMat<T>::newsize(size_t ni_size, size_t nj_size)
{
  v_.resize(ni_size * nj_size);

  i_size = ni_size;
  j_size = nj_size;

  return *this;
}

template<class T>
Vector<T> average0(const ColMat<T> &a)
{
  /* average along rows */

  size_t nr = a.size(0), nc = a.size(1);
  Vector<T> av(nc,0);
  
  for (size_t j=0; j<nc; ++j)
    for (size_t i=0; i<nr; ++i)
      av(j) += a(i,j);

  return av/T(nr);
}

template<class T>
Vector<T> average1(const ColMat<T> &a)
{
  /* average along columns */
  
  size_t nr = a.size(0), nc = a.size(1);
  Vector<T> av(nr,0);
  
  for (size_t i=0; i<nr; ++i)
    for (size_t j=0; j<nc; ++j)
      av(i) += a(i,j);

  return av/T(nc);
}

template<class T>
Vector<T> sum0(const ColMat<T> &a)
{
  /* sum row elements (vertically) */
  
  size_t nr = a.size(0), nc = a.size(1);
  Vector<T> av(nc,0);

  for (size_t j=0; j<nc; ++j)
    {
      for (size_t i=0; i<nr; ++i)
	{
	  av(j) += a(i,j);
	}
    }
  return av;
}

template<class T>
Vector<T> sum1(const ColMat<T> &a)
{
  size_t ni = a.isize(), nj = a.jsize();
  Vector<T> av(ni,0);
  
  for (size_t i = nj; i--;)
    for (size_t j = ni; j--;)
      av(i) += a(i,j);

  return av;
}

template<class T> 
ColMat<T> MatMult(const ColMat<T> &a, const ColMat<T> &b)
{
  size_t i,j,k;
  ColMat<T> c(a.size(0), b.size(1), 0.0);

#ifndef NO_ASSERT
  assert(("ColMat<T> MatMult(const ColMat<T> &a, const ColMat<T> &b): "
	  "incompatible sizes of matrixes",
	  a.jsize() == b.isize()));
#endif

  for (j=0; j<c.size(1); ++j)
    for (i=0; i<c.size(0); ++i)
      for (k=0; k<a.size(1); k++)
	c(i,j) += a(i,k) * b(k,j);
  
  return c;
}

template<class T> 
Vector<T> MatMult(const ColMat<T> &a, const Vector<T> &b)
{
  size_t i,j;
  Vector<T> c(a.size(0),0.);

  size_t n = c.size();
  size_t n1 = a.size(1);
  for (i=0; i<n; ++i)
    for (j=0; j<n1; ++j)
      c(i) += a(i,j) * b(j);

  return c;
}

template<class T> 
Vector<T> ScalProd(const ColMat<T> &a, const Vector<T> &b)
{
  return MatMult(a, b);
}

template<class T> 
Vector<T> ScalProd(const Vector<T> &b, const ColMat<T> &a)
{
  size_t i,j;
  Vector<T> c(a.size(1),0);
  
  size_t n = c.size();
  size_t n0 = a.size(0);
  for (j=0; j<n; ++j)
    for (i=0; i<n0; ++i)
      c(j) += b(i)*a(i,j);

  return c;
}


template<class T> 
ColMat<T> TripleMatMult(const ColMat<T> &a, const ColMat<T> &b, 
			   const ColMat<T> &d)
{
  size_t i,j,k,m;
  size_t n1 = a.size(0), n2 = b.size(0), n3 = d.size(0), n4 = d.size(1);

  ColMat<T> c(n1,n4, 0.0);

  for (j=0; j<n4; ++j)
    {
      for (i=0; i<n1; ++i)
	{
	  for (k=0; k<n2; k++)
	    {
	      for (m=0; m<n3; m++) c(i,j) = c(i,j) + a(i,k) * b(k,m) * d(m,j);
	    }
	}
    }
  return c;
}

template<class T> 
ColMat<T> MatMult(const ColMat<T> &a, const ColMat<T> &b, const ColMat<T> &c)
{
  ColMat<T> d = TripleMatMult( a, b, c );
  return d;
}

/*
 multiplication element by element
 */
     
template<class T> 
ColMat<T> operator*(const ColMat<T> &a, const ColMat<T> &b)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> operator*(const ColMat<T> &a, const ColMat<T> &b): " \
	  "different sizes of matrixes",
	  a.isize() == b.isize() && a.jsize() == b.jsize()));
#endif

  ColMat<T> c(a);
  return c*=b;
}
 
/*
 /  division
 */
     
template<class T> 
ColMat<T> operator/(const ColMat<T> &a, const T f)
{
  T g=1/f;
  return a * g;
}

template<class T> 
ColMat<T> operator/(const T f, const ColMat<T> &a)
{
  ColMat<T> c(a.size(0), a.size(1), f);
  return c/=a;
}
/*
 / element by element division
 */

template<class T> 
ColMat<T> ColMat<T>::operator/=(const T f) 
{
  transform(v_.begin(), v_.end(), v_.begin(), bind2nd(divides<T>(), f));
  return *this;
}
template<class T> 
ColMat<T> ColMat<T>::operator/=(const ColMat<T> &a) 
{
#ifndef NO_ASSERT
  assert(("ColMat<T> ColMat<T>::operator/=(const ColMat<T> &a): " \
	  "different sizes of matrixes",
	  isize() == a.isize() && jsize() == a.jsize()));
#endif

  transform(v_.begin(), v_.end(), a.v_.begin(), v_.begin(), divides<T>());
  return *this;
}

template<class T>
ColMat<T> operator/(const ColMat<T> &a, const ColMat<T> &b)
{
#ifndef NO_ASSERT
  assert(("ColMat<T> operator/(const ColMat<T> &a, const ColMat<T> &b): " \
	  "different sizes of matrixes",
	  a.isize() == b.isize() && a.jsize() == b.jsize()));
#endif
  
  ColMat<T> c(a);
  return c/=b;
}

template<class T> ColMat<T> sin(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( sin ));
  return b;  
}  

template<class T> ColMat<T> cos(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( cos ));
  return b;  
}  

template<class T> ColMat<T> tan(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( tan ));
  return b;  
}  


template<class T> ColMat<T> asin(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( asin ));
  return b;  
}  

template<class T> ColMat<T> acos(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( acos ));
  return b;  
}  

template<class T> ColMat<T> atan(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( atan ));
  return b;  
}  

template<class T> ColMat<T> exp(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( exp ));
  return b;  
}  

template<class T> ColMat<T> log(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( log ));
  return b;  
}  

template<class T> ColMat<T> sqrt(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( sqrt ));
  return b;  
}  

template<class T> ColMat<T> abs(const ColMat<T> &a)  
{ 
  ColMat<T> b(a.size(0), a.size(1)); 
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), static_cast<T(*)(T)>( abs ));
  return b;  
}  

template<class T> ColMat<T> pow(const ColMat<T> &a, const T exp)  
{ 
  ColMat<T> b(a.size(0), a.size(1));  
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), bind2nd(Power<T>(), exp));  
  return b;  
}  

template<class T> ColMat<T> pow(const ColMat<T> &a, const int exp)  
{ 
  // there ought to be a better way
  return pow(a, static_cast<T>(exp));
}  

template<class T> 
ColMat<T> space1(T xmin, T xmax, size_t ny, size_t nx) 
{
  size_t i,j;
  ColMat<T> a(ny,nx);

  for (i=0; i<a.size(0); ++i) 
    {
      for (j=0; j<a.size(1); ++j)
	{
	  a(i,j) = xmin + (xmax-xmin)*T(j)/T(a.size(1)-1);
	}
    }
  return a;

}

template<class T> 
ColMat<T> space0(T ymin, T ymax, size_t ny, size_t nx) 
{
  size_t i,j;
  ColMat<T> a(ny,nx);

  for (i=0; i<a.size(0); ++i) 
    {
      for (j=0; j<a.size(1); ++j)
	{
	  a(i,j) = ymin + (ymax-ymin)*T(i)/T(a.size(0)-1);
	}
    }
  return a;

}

/* stretch vertically */

template<class T> 
ColMat<T> stretch0(const  Vector<T> &v, const size_t n0)
{  
  size_t nv = v.size();
  ColMat<T> d(n0, nv); 
  
  for(size_t i=0; i<n0; ++i) 
    for (size_t j=0; j<nv; ++j) 
      d(i,j) = v(j);
 
  return d; 
}  
  
/* stretch horizontally */

  
template<class T>
ColMat<T> stretch1(const Vector<T> &v, const size_t n1)
{  
  size_t nv = v.size();
  ColMat<T> d(nv, n1); 

  for(size_t j=0; j<n1; ++j) 
    for (size_t i=0; i<nv; ++i) 
      d(i,j) = v(i);
  
  return d; 
}  

/* tensor product of 2 vectors = matrix */

template<class TYPE>
ColMat<TYPE> TensorProd(const Vector<TYPE> &a, const Vector<TYPE> &b){
  size_t m = a.size(), n = b.size();
  ColMat<TYPE> c(m,n);
  for(size_t j=0; j < n; j++){
    for(size_t i=0; i < m; i++) c(i,j) = a(i)*b(j);
  }
  return c;
}

/* convert matrix to vector by flattenning along columns or rows */

template<class TYPE>
Vector<TYPE> flatten0(const ColMat<TYPE> &a){
  size_t m = a.size(0), n = a.size(1);
  Vector<TYPE> r(m*n);
  for(size_t j=0; j<n; j++){
    for(size_t i=0; i<m; i++) r(j*m+i) = a(i,j);
  }
  return r;
}

template<class TYPE>
Vector<TYPE> flatten1(const ColMat<TYPE> &a){
  size_t m = a.size(0), n = a.size(1);
  Vector<TYPE> r(m*n);
  for(size_t j=0; j<n; j++){
    for(size_t i=0; i<m; i++) r(i*n+j) = a(i,j);
  }
  return r;
}

template<class T>
ColMat<T> ColMat<T>::operator()(const Vector<size_t> &I, const Vector<size_t> &J ) const
{
  size_t nr = I.size();
  size_t nc = J.size();

  ColMat<T> y(nr, nc);
  
  for( size_t j=0; j<nc; ++j)
    for( size_t i=0; i<nr; ++i)
      y(i, j) = (*this)(J(j), I(i));

  return y;
}

template<class T>
Vector<T> ColMat<T>::operator()(const Vector<size_t> &I, const size_t j ) const 
{
  size_t n = I.size();
  Vector<T> y(n);

  for( size_t i=0; i<n; ++i)
    y(i) = v_[j*i_size + I(i)];
  
  return y;
}

template<class T>
Vector<T> ColMat<T>::operator()(const size_t i, const Vector<size_t> &J ) const
{
  size_t n = J.size();
  Vector<T> y(n);
  for( size_t j=0; j<n; ++j)
    y(j) = v_[J(j)*i_size + i];

  return y;
}

template<class T>
size_t ColMat<T>::size(size_t i) const 
{
#ifndef NO_ASSERT
  assert(("ColMat<T>::size(size_t i): i can only be 0 or 1", i==0 || i==1));
#endif

  return i ? j_size : i_size;
}

/* 
slip vector into matrix. 
absorb vector elements of v into matrix. Each element of v(i) 
takes a matrix slot at I(i) and J(i), i = 1,...v.size().
   */

template<class T>
ColMat<T> ColMat<T>::slip(const Vector<T> &v, const Vector<size_t> &I, const Vector<size_t> &J)
{
  size_t nv = v.size();

#ifndef NO_ASSERT
  assert(nv == I.size() && nv == J.size() );
#endif

  for(size_t i = 0; i < nv; ++i)
    v_(J(i) * i_size + I(i)) = v(i);

  return *this;
}

template<class T>
ColMat<T> ColMat<T>::slip(const Vector<T> &v, const Vector<size_t> &I, const size_t j)
{
  size_t nv = v.size();

#ifndef NO_ASSERT
  assert( nv == I.size() );
#endif

  for(size_t i=0; i < nv; ++i)
    v_(j*i_size + I(i)) = v(i);

  return *this;
}

template<class T>
ColMat<T> ColMat<T>::slip(const Vector<T> &v, const size_t i, const Vector<size_t> &J)
{
  size_t nv = v.size();

#ifndef NO_ASSERT
  assert( nv == J.size() );
#endif

  for(size_t j=0; j<nv; ++j) v_(J(j)*i_size + i) = v(j);
  return *this;
}

template<class T>
T min(const ColMat<T> &a)
{
  return *min_element(a.v_.begin(), a.v_.end());
}

template<class T>
T max(const ColMat<T> &a)
{
  return *max_element(a.v_.begin(), a.v_.end());
}

template<class T>
ColMat<T> min(const ColMat<T> &a, const T b)
{
  ColMat<T> c(a.size(0), a.size(1));
  transform(a.v_.begin(), a.v_.end(), c.v_.begin(), bind2nd(Min<T>(), b));
  return c;
}

template<class T>
ColMat<T> max(const ColMat<T> &a, const T b)
{
  ColMat<T> c(a.size(0), a.size(1));
  transform(a.v_.begin(), a.v_.end(), c.v_.begin(), bind2nd(Max<T>(), b));
  return c;
}

template<class T>
ColMat<T> min(const ColMat<T> &a, const ColMat<T> &b)
{
  ColMat<T> c(a.size(0), a.size(1));
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), c.v_.begin(), Min<T>());
  return c;
}

template<class T>
ColMat<T> max(const ColMat<T> &a, const ColMat<T> &b)
{
  ColMat<T> c(a.size(0), a.size(1));
  transform(a.v_.begin(), a.v_.end(), b.v_.begin(), c.v_.begin(), Max<T>());
  return c;
}

#endif /* _MATRIX_H */
