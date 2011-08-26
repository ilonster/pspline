/* -*-C++-*- */
/* Created by A. Pletzer, Oct 23 1998, 2002
 * Changes by Irek Szczesniak, July 20 2001:
 *   - switched to the STL vector,
 *   - defined a new class Vector,
 */

#ifndef _VECTORF_H_
#define _VECTORF_H_    

#ifndef NO_ASSERT
#include <assert.h>
#endif

#include <math.h>
#include <stdlib.h>

// STL
#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>

#include <sys/types.h>
#include <time.h>

using namespace std;

#include "Functors.h"


/** The vector class

 The class to represent vectors.  It is guaranteed that adjacent
 matrix elements are adjecent in memory. */

template<class T>
class Vector : public vector<T>
{                         
public:
  typedef typename vector<T>::size_type size_type;

  /*:::::::::::::::*/
  /* Constructors  */
  /*:::::::::::::::*/

  /** The constructor with no arguments.  After using this constructor,
    the "alloc" function must be used. */
  Vector(); 
  
  /** A constructor with a specification of the number of elements of
    the new vector.
    
    @param n vector size */
  Vector(size_type n); 

  /** A constructor with a specification of the number of elements of
    the new vector.  The vector will be filled with elements set to "e".
    
    @param n vector size 
    @param e element to be used in filling in*/
  Vector(size_type n, const T &e); 

  /** The copy constructor. Elements are copied into a new vector. 
   @param vector to be copied */
  Vector(const Vector<T> &);

  /** The assignment operator. Set all elements to f. 
   @param f scalar
   @return vector instance */
  Vector<T> &operator=(const T f);

  /** The operator adds the value of "f" to every element of the vector. 
   @param f scalar 
   @return vector */
  Vector<T> &operator+=(const T f);

  /** The operator subtracts the value of "f" to every element of the
      vector. 
  @param f scalar 
  @return vector */
  Vector<T> &operator-=(const T f);

  /** The operator multiplies every element of the vector by the value
      of "f". 
  @param f scalar
  @return vector */
  Vector<T> &operator*=(const T f);

  /** The operator divides every element of the vector by the value of
      "f". 
  @param f scalar
  @return vector */
  Vector<T> &operator/=(const T f);

  /** Vector addition. 
   @param w vector
   @return vector = original vector incremented by w */
  Vector<T> &operator+=(const Vector<T> &w);

  /** Vector subtraction. 
   @param w vector
   @return vector = original vector decremented by w  */
  Vector<T> &operator-=(const Vector<T> &w);

  /** Elementwise multiplication. 
      The operator that multiplies every vector element (of the vector
    on the left of "*=") by its counterpart in the "b" vector.
  @param w vector
  @return vector */
  Vector<T> &operator*=(const Vector<T> &w);

  /** Elementwise division. 
    The operator that divides every vector element (of the vector on
    the left of "*=") by its counterpart in the "b" vector.
  @param w vector
  @return vector  */
  Vector<T> &operator/=(const Vector<T> &w);

  /** Uniform grid generation. Elements are set so as to 
      span uniformly from xmin to xmax, in increasing order.
      @param xmin minimum value
      @param xmax maximum value
  */
  void space(const T xmin, const T xmax);

  /** Uniform grid generation with unit increment. Similar to space except that
      the increment is one. For instance x.range(0) has elements 0, 1, ... (up
      to size x.size()-1).
      @see space
  */
  void range(const T imin=0);

  /** Fills the vector with random numbers between 0 and 1. 
      Two subsequent calls to random will generate different
      elements. However, a program will consistently generate the same numbers.
  */
  void random(void);

  /** Return index of element closest to the left of/at position of elem. 
      This assumes that the sequence is monotonically increasing. The 
      min/max return values are 0 and size()-1, respectively.
   @param elem reference abscissa
   @see ket
  */

  size_t bra(const T elem)
    {
      T *beg = &this->operator[](0);
      T *end = &this->operator[](this->size());
      T *i = upper_bound(beg, end, elem);
      return static_cast<size_t>( (i>beg)? i-beg-1 : 0 );
    }

  /** Return indices of elements closest to the left of elem. 
      This assumes that the sequence is monotonically increasing. 
   @param elem reference abscissa
  */

  Vector<size_t> bra(const Vector<T> &elem);

  
  /** Return index of element closest to the right of elem. 
      This assumes that the sequence is monotonically increasing. The
      min/max return values are 0 and size()-1, respectively.      
   @param elem reference abscissa
   @see bra
  */

  size_t ket(const T elem){
    size_t n = this->size();
    size_t i = this->bra(elem) + 1;
    return static_cast<size_t>( (i<n)? i : n-1 );
  }

  /** Return indices of elements closest to the right of elem. 
      This assumes that the sequence is monotonically increasing. 
   @param elem reference abscissa
  */

  Vector<size_t> ket(const Vector<T> &elem);

  /** Resizes the vector to the "n" size. 
   @param n new size
  */

  Vector<T> &alloc(const size_type n);

  /*::::::::::::::::::::::::::::::::*/                           
  /*  Indices and access operations */                           
  /*::::::::::::::::::::::::::::::::*/                           

  /** Return reference to "i"-th element.  
   @param i index 
   @return element */
  inline const T& operator()(size_type i) const
  {
#ifndef NO_ASSERT
    assert(i < this->size());
#endif

    return (*this)[i];
  }

  /** Returns reference to an element with the "i" index. Also used for assignment. 
   @param i index 
   @return element */
  inline T& operator()(size_type i)
  {
#ifndef NO_ASSERT
    assert(i < this->size());
#endif
    
    return (*this)[i];
  }

  Vector<T> operator()(const Vector<size_t> &I) const;
};

typedef Vector<double> Vec;
typedef Vector<size_t> Vec_int;

/**@name Vector Functions
  These are global functions operating on or generating vectors.  */

//@{

/** Addition. 
 @param v a vector
 @param w another vector
 @param vector = v+w */
template<class T>
Vector<T> operator+(const Vector<T> &v, const Vector<T> &w);

/** Subtraction. 
 @param v a vector
 @param w another vector
 @param vector = v-w */
template<class T>
Vector<T> operator-(const Vector<T> &v, const Vector<T> &w);

/** Elementwise multiplication. Not to be confused with the dot-product ScalProd.
 @see ScalProd
 @param v a vector
 @param w another vector
 @param vector = v*w (!= ScalProd(v, w))*/
template<class T>
Vector<T> operator*(const Vector<T> &v, const Vector<T> &w);

/** Elementwise division. 
 @see ScalProd
 @param v a vector
 @param w another vector
 @param vector = v/w */
template<class T>
Vector<T> operator/(const Vector<T> &v, const Vector<T> &w);

/** (Left) addition with a scalar. This is equivalent to creating a vector filled 
    with "f" and adding it with "a".
    @param f a scalar
    @param a a vector
    @return vector = f + a
*/
template<class T>
Vector<T> operator+(const T f, const Vector<T> &a);

/** (Right) addition with a scalar. This is equivalent to creating a vector filled 
    with "f" and adding it with "a".
    @param a a vector
    @param f a scalar
    @return vector = a + f*/
template<class T>
Vector<T> operator+(const Vector<T> &a, const T f);

/** Subtraction from a scalar. This is equivalent to creating a vector filled with
    "f" and subtracting from it "w".
    @param f a scalar
    @param w a vector
    @return vector = f-w
*/
template<class T>
Vector<T> operator-(const T f, const Vector<T> &w);

/** (Left) multiplication of a vector by the scalar "f". 
 @param f s scalar
 @param a a vector
 @return vector = f*a
*/
template<class T>
Vector<T> operator*(const T f, const Vector<T> &a);

/** (Right) multiplication of a vector by the scalar "f". 
 @param a a vector
 @param f s scalar
 @return vector = f*a
*/
template<class T>
Vector<T> operator*(const Vector<T> &a, const T f);

/** Elementwise division. 
    @param f a scalar
    @param a a vector
    @return vector whose elements are f / element of "a"
*/
template<class T>
Vector<T> operator/(const T f, const Vector<T> &a);

/** Scalar product. This is equivalent to sum(v*w). Not to be confused with the
 elementwise product v*w. 
@param v a vector
@param w another vector
@return vector = v.w
*/
template<class T>
T ScalProd(const Vector<T> &v, const Vector<T> &w);

/** Apply function "sin" to each element. 
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> sin(const Vector<T> &v);

/** Apply function "cos" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> cos(const Vector<T> &v);

/** Apply function "tan" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> tan(const Vector<T> &v);

/** Apply function "asin" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> asin(const Vector<T> &v);

/** Apply function "acos" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> acos(const Vector<T> &v);

/** Apply function "atan" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> atan(const Vector<T> &v);

/** Apply function "exp" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> exp(const Vector<T> &v);

/** Apply function "log" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> log(const Vector<T> &v);

/** Apply function "sqrt" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> sqrt(const Vector<T> &v);

/** Apply function "abs" to each element.
    @param v a vector
    @return vector 
   */
template<class T>
Vector<T> abs(const Vector<T> &v);

/** Apply function "pow" to each element.
    @param v a vector
    @param exp the exponent
    @return vector 
 */
template<class T>
Vector<T> pow(const Vector<T> &v, const T exp);

/** Apply function "pow" to each element.
    @param v a vector
    @param exp the exponent
    @return vector 
 */
template<class T>
Vector<T> pow(const Vector<T> &v, const int exp);

/** Uniform grid generation. Create a vector of size n whose elements uniformly span 
    from xmin to xmax. 
    @param xmin 
    @param xmax
    @param n
 */
template<class T>
Vector<T> space(const T xmin, const T xmax, const size_t n);

/** Uniform grid generation with integer increment. Create vector of size nsize whose
    elements are imin, imin+1, ... imin+size()-1.
    @see space
    @param imin
    @param nsize
    @return vector = [imin, imin+1, ... imin+size()-1]
 */
template<class T>
Vector<T> range(const T imin=0, const size_t nsize=2);

/** Return the maximum value of v. 
    @param vector 
    @return scalar = max(v)
 */
template<class T>
T max(const Vector<T> &v);

/** Comparative maximum operator. Return the vector that takes the max 
    values between each element of v and w.
    @param v a vector
    @param w another vector
    @return vector = max(v, w)
 */
template<class T>
Vector<T> max(const Vector<T> &v, const Vector<T> &w);

/** Comparative maximum operator. Return the vector that takes the max 
    values between each element of v and a scalar f.
    @param v a vector
    @param f a scalar    
    @return vector = max(v, f)
 */
template<class T>
Vector<T> max(const Vector<T> &v, const T f);

/** Comparative maximum operator. Return the vector that takes the max 
    values between each element of v and a scalar f.
    @param f a scalar    
    @param v a vector
    @return vector = max(f, v)
 */
template<class T>
Vector<T> max(const T f, const Vector<T> &v1);

/** Return the min value of v.
    @param vector 
    @return scalar = min(v)
  */
template<class T>
T min(const Vector<T> &v);

/** Comparative minimum operator. Return the vector that takes the min
    values between each element of v and w.
    @param v a vector
    @param w another vector
    @return vector = min(v, w)
 */
template<class T>
Vector<T> min(const Vector<T> &v, const Vector<T> &w);

/** Comparative minimum operator. Return the vector that takes the min
    values between each element of v and a scalar f.
    @param v a vector
    @param f a scalar    
    @return vector = min(v, f)
 */
template<class T>
Vector<T> min(const Vector<T> &v, const T f);

/** Comparative minimum operator. Return the vector that takes the min 
    values between each element of v and a scalar f.
    @param f a scalar    
    @param v a vector
    @return vector = min(f, v)
 */
template<class T>
Vector<T> min(const T f, const Vector<T> &v);

/** Sum all elements. 
    @see ScalProd
    @param v a vector
    @return scalar = contraction of v.
 */
template<class T>
T sum(const Vector<T> &v);

/** Vector concatenation. 
    @param v a vector
    @param w another vector
    @return vector = concatenation of v and w
 */
template<class T>
Vector<T> cat( const Vector<T> &v, const Vector<T> &w);


/** Overloading of <<. For printing purposes. 
 */
template <class T>
ostream& operator<<(ostream& s, const Vector<T>& V);

//@}

/****************************************************************************/
// Definition of the Vector Class
/****************************************************************************/

template<class T>
Vector<T>::Vector()
{
}

template<class T>
Vector<T>::Vector(size_type n) : vector<T>(n)
{
}

template<class T>
Vector<T>::Vector(size_type n, const T &e) : vector<T>(n, e)
{
}

template<class T>
Vector<T>::Vector(const Vector<T> &w)
{
  (*this) = w;
}

template<class T>
void Vector<T>::space(const T xmin, const T xmax)
{
  for (size_t i = 0; i < this->size(); ++i)
    (*this)[i] = xmin + (xmax-xmin)*T(i)/T(this->size()-1);
}

template<class T> 
void Vector<T>::random(void) 
{
  transform(this->begin(), this->end(), this->begin(), Random<T>());
}

template<class T>
void Vector<T>::range(const T imin) 
{
  for (size_t i = this->size(); i--;){
    (*this)[i] = imin + T(i);
  }
}


template<class T>
Vector<T> &Vector<T>::alloc(const size_type n)
{
  resize(n);

  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator=(const T f) 
{
  transform(this->begin(), this->end(), this->begin(), Setval<T>(f));
  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator+=(const T f) 
{
  transform(this->begin(), this->end(), this->begin(), bind2nd(plus<T>(), f));
  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator+=(const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(this->size() == w.size());
#endif
  transform(this->begin(), this->end(), 
	    w.begin(), 
	    this->begin(), 
	    plus<T>() );
  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator-=(const Vector<T> &w)
{
#ifndef NO_ASSERT
  assert(this->size() == w.size());
#endif
  transform(this->begin(), this->end(), 
	    w.begin(), 
	    this->begin(), 
	    minus<T>() );

  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator-=(const T f) 
{ 
  transform(this->begin(), this->end(), this->begin(), bind2nd(minus<T>(), f));
  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator*=(const T f) 
{ 
  transform(this->begin(), this->end(), this->begin(), bind2nd(multiplies<T>(), f));
  return *this;  
}

template<class T>
Vector<T> &Vector<T>::operator*=(const Vector<T> &w)
{  
#ifndef NO_ASSERT
  assert(this->size() == w.size());
#endif
  transform(this->begin(), this->end(), 
	    w.begin(), 
	    this->begin(), 
	    multiplies<T>() );
  
  return (*this);  
}

template<class T>
Vector<T> &Vector<T>::operator/=(const T f) 
{
  transform(this->begin(), this->end(), this->begin(), bind2nd(divides<T>(), f));
  return (*this);
}

template<class T>
Vector<T> &Vector<T>::operator/=(const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(this->size() == w.size());
#endif
  transform(this->begin(), this->end(), 
	    w.begin(), 
	    this->begin(), 
	    divides<T>() );

  return (*this);
}


template<class T>
Vector<T> Vector<T>::operator()(const Vector<size_t> &I) const
{
  Vector<T> y(I.size());

  for (size_t i = I.size(); i--;)
    y[i] = (*this)[ I[i] ];
  
  return y;
}

// Definition of extra functions supporting the Vector class

// find max element

template<class T>
T max(const Vector<T> &v)
{
#ifndef NO_ASSERT
  assert(v.size()>0);
#endif
  return *max_element(v.begin(), v.end());
}

// find min element 

template<class T>
T min(const Vector<T> &v) 
{
#ifndef NO_ASSERT
  assert(v.size()>0);
#endif
  return *min_element(v.begin(), v.end());
}

// the + operator

template<class T>
Vector<T> operator+(const Vector<T> &v, const T f) 
{
  Vector<T> c(v);
  return c += f;
}

template<class T>
Vector<T> operator+(const T f, const Vector<T> &v) 
{
  Vector<T> c(v);
  return c += f;
}

template<class T>
Vector<T> operator+(const Vector<T> &v, const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(v.size()==w.size());
#endif
  Vector<T> c(v);
  return c += w;
}

template<class T>
Vector<T> operator-(const Vector<T> &v, const T f)
{
  Vector<T> c(v);
  return c -= f;
}

template<class T>
Vector<T> operator-(const T f, const Vector<T> &v)
{
  Vector<T> c = -v;
  return c += f;
}

template<class T>
Vector<T> operator-(const Vector<T> &v, const Vector<T> &w)
{
  Vector<T> c(v);
  return c -= w;
}

template<class T>
Vector<T> operator-(const Vector<T> &v)
{
  Vector<T> c(v.size());
  transform(v.begin(), v.end(), c.begin(), negate<T>());

  return c;
}

template<class T>
T ScalProd(const Vector<T> &v, const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(v.size() == w.size());
#endif
  return inner_product(v.begin(), v.end(), w.begin(), static_cast<T>(0));
}

template<class T>
Vector<T> operator*(const Vector<T> &v, const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(v.size() == w.size());
#endif

  Vector<T> c(v);
  return c*=w;
}

template<class T>
Vector<T> operator*(const T f, const Vector<T> &v)
{
  Vector<T> c(v.size(), f);
  return c *= v;
}

template<class T>
Vector<T> operator*(const Vector<T> &v, const T f)
{
  Vector<T> c(v.size(), f);
  return c *= v;
}

template<class T>
Vector<T> operator/(const Vector<T> &v, const T f) 
{
  Vector<T> c(v);
  return c/=f;
}

template<class T>
Vector<T> operator/(const T f, const Vector<T> &v) 
{
  Vector<T> c(v.size());
  transform(v.begin(), v.end(), c.begin(), bind1st(divides<T>(), f));

  return c;
}

// element by element division

template<class T>
Vector<T> operator/(const Vector<T> &v, const Vector<T> &w) 
{
  Vector<T> c(v);

  return c/=w;
}

// Math functions

template<class T>
Vector<T> sin(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( sin ) );

  return b;  
}  

template<class T>
Vector<T> cos(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( cos ) );

  return b;  
}  

template<class T>
Vector<T> tan(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( tan ) );

  return b;  
}  

template<class T>
Vector<T> asin(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( asin ) );

  return b;  
}  

template<class T>
Vector<T> acos(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( acos ) );

  return b;  
}  

template<class T>
Vector<T> atan(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( atan ) );

  return b;  
}  

template<class T>
Vector<T> exp(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( exp ) );
  
  return b;  
}  

template<class T>
Vector<T> log(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( log ) );

  return b;  
}  

template<class T>
Vector<T> sqrt(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( sqrt ) );

  return b;  
}
  
template<class T>
Vector<T> abs(const Vector<T> &v) 
{ 
  Vector<T> b(v.size());  
  transform(v.begin(), v.end(), b.begin(), static_cast<T(*)(T)>( fabs ) );

  return b;  
}  

template<class T>
Vector<T> pow(const Vector<T> &v, const T exp) 
{ 
  Vector<T> b(v.size()); 
  transform(v.begin(), v.end(), b.begin(), bind2nd(Power<T>(), exp));  
  return b;  
}  

template<class T>
Vector<T> pow(const Vector<T> &v, const int exp) 
{ 
  // we've got to find a more efficient way to do this...
  return pow(v, static_cast<T>(exp));
}  

// grid generator
    
template<class T>
Vector<T> space(const T xmin, const T xmax, const size_t n = 2)
{
  Vector<T> a(n);

  for (size_t i = n; i--;)
    a[i] = xmin + (xmax - xmin)*T(i)/T(n - 1);

  return a;
}
    
template<class T>
Vector<T> range(const T imin, const size_t n=2) 
{
  Vector<T> a(n);

  for (size_t i = n; i--;)
    a[i] = imin + T(i);

  return a;
}


// max of 2 vectors

template<class T>
Vector<T> max(const Vector<T> &v, const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(v.size() == w.size());
#endif
  size_t n = v.size();
  Vector<T> res(n);
  for (size_t i = 0; i < n; ++i)  
    res[i] = v(i) > w(i) ? v(i) : w(i);
  
  return res;
}

template<class T> 
Vector<T> max(const Vector<T> &v, const T f) 
{
  Vector<T> res(v.size());
  transform(v.begin(), v.end(), res.begin(), bind2nd(Max<T>(), f));
  
  return res;
}

template<class T>
Vector<T> max(const T f, const Vector<T> &v) 
{
  return max(v, f);
}

// min of 2 arguments

template<class T> Vector<T>  min(const Vector<T> &v, const Vector<T> &w) 
{
#ifndef NO_ASSERT
  assert(v.size() == w.size());
#endif
  Vector<T> res(v.size());
  transform(v.begin(), v.end(), w.begin(), 
	    res.begin(), min<T>());
  
  return res;
}

template<class T> Vector<T>  min(const Vector<T> &v, const T f) 
{
  Vector<T> res(v.size());
  transform(v.begin(), v.end(), res.begin(), bind2nd(Min<T>(), f));
  
  return res;
}

template<class T>
Vector<T> min(const T f, const Vector<T> &v) 
{
  return min(v, f);
}

/*
sum of elements
*/

template<class T>
T sum(const Vector<T> &v) 
{
  return accumulate(v.begin(), v.end(), static_cast<const T>(0));
}

// concatenate two Vectors

template<class T> Vector<T> cat( const Vector<T> &v1, const Vector<T> &v2) 
{
  size_t n1 = v1.size(); 
  size_t n2 = v2.size();
  Vector<T> res(n1+n2);

  for (size_t i=0; i<n1; ++i) res(i   ) = v1(i);
  for (size_t j=0; j<n2; ++j) res(j+n1) = v2(j);

  return res;
}

// class methods to fill up elements of an existing Vector

template<class T>
void space(Vector<T> &v, const T xmin, const T xmax) 
{
#ifndef NO_ASSERT
  assert(v.size() >= 2);
#endif

  size_t n = v.size();

  for (size_t i = n; i--;)
    v[i] = xmin + (xmax - xmin)*T(i)/T(n - 1);
}
    
template<class T>
void range(Vector<T> &v, const T imin) 
{
  for (size_t i = v.size(); i--;)
    v[i] = T(i) + imin;
}

// return index of min(v)

template<class T>
size_t index_min(Vector<T> &v)
{
  T *beg = &v[0];
  T *end = &v[v.size()];  
  T *i = min_element(beg, end);
  return static_cast<size_t>(i - beg);
}

// return index of max(v)

template<class T>
size_t index_max(Vector<T> &v)
{
  T *beg = &v[0];
  T *end = &v[v.size()];  
  T *i = max_element(beg, end);
  return static_cast<size_t>(i - beg);
}


//*******************************************

template <class T>
ostream& operator<<(ostream& s, const Vector<T>& v)
{
  for_each(v.begin(), v.end(), Print<T>(s));
  return s;
}

#endif /* _VECTORF_H_ */
