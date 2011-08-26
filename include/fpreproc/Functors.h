// -*-C++-*- 

#ifndef _FUNCTORS_H_
#define _FUNCTORS_H_

#include <functional>

#include <iostream>

// List of functors for Vector and Matrix classes

template<class T> struct Random : public unary_function<T, T>{
  Random() : max(static_cast<T>(RAND_MAX)) {}
  T operator() (T x) {return static_cast<T>(rand())/max;}
  T max;
};

template<class T> struct Setval : public unary_function<T, T>
{
  Setval(T fin) : f(fin) {};
  T operator() (T x) {return f;}
  T f;
};

template<class T> struct Power : public binary_function<T, T, T> {
  T operator() (T x, T y) const { return pow(x, y); }
};

template<class T> struct Max : public binary_function<T, T, T> {
  T operator() (T x, T y) const { return max<T>(x, y); }
};

template<class T> struct Min : public binary_function<T, T, T> {
  T operator() (T x, T y) const { return min<T>(x, y); }
};

template<class T> struct Print : public unary_function<T, void>
{
  ostream& os;
  Print(ostream& out) : os(out) {
    os.precision(13);
  }
  void operator() (T x) { os << x << ' '; }
};


#endif
