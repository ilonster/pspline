#ifndef RANGE_H
#define RANGE_H

//-----------------------------------------------------------------------------
//
//  Peter Richter (prichter@princeton.edu)
//  Princeton Plasma Physics Laboratory
//  August 2000
//
//  range.h - Range class
//
//-----------------------------------------------------------------------------

#include "iutil.h"
#include "state.h"

#include <string> 
#include <iostream> 

using namespace std ;  // pollution


//
// ----------------------- Range ---------------------------
// defines the valid range of one dependent variable in a 
// reaction.  The policy decides how to handle range errors
// and check() methods exist for implementing the policy.
//

class Range {
public:
  //
  // policy for handling out of range errors on axis
  //
  enum policy {IGNORE=0, WARN=1, HALT=2, IGNORE_CLEAR=3, WARN_CLEAR=4} ;

  //
  // describes whether the range is based on a log or linear
  // scale.  This has implications on how the minimum and
  // maximum range values are compared for closeness.
  //
  enum scaling {LOG=0, LINEAR=1} ;

  Range(string="") ;  // supply a descriptive label for this range,
                      // default limits are 0,0 and ignore policy

  // -- modify --
  void set_name(string name) { label=name ; } ;  // change the name
  void set_scale(scaling s) ; // LOG scale will force limits>0

  void set_min(fp_64 min) ;   // modify limits -- keep minimum<=maximum
  void set_max(fp_64 max) ;

  // modify the policy and value to clear with if the range
  // is exceeded.  The clear_scale should be set to the relative
  // scale of the clear_value and must be greater then zero otherwise
  // it will not be set -- the same clear_scale is used for the  
  // minimum clear value and the maximum clear value.
  void set_min_policy(policy pmin, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;
  void set_max_policy(policy pmax, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;

  // -- probe --
  string   name()  const { return label ;  } ;
  scaling  scale() const { return rscale ; } ;

  fp_64 min() const { return minimum ; } ; 
  fp_64 max() const { return maximum ; } ; 

  policy min_policy() const { return minimum_policy ; } ;
  policy max_policy() const { return maximum_policy ; } ;

  fp_64 min_clear()   const { return clear_minimum ; } ;
  fp_64 max_clear()   const { return clear_maximum ; } ;

  fp_64 clear_scale() const { return clear_scale_minmax ; } ;

  int query(const fp_64& x) const ;   //  0 -> in bounds
                                      // -1 -> out of bounds and would throw an exception
                                      //  1 -> out of bounds but no exception (warn or ignore)

  state current() const ;  // return a description of the current state

  // --- check ---
  // The following checks will throw an exception on halt policy
  // or return true if there was a warning.  The functions will return
  // false if there were no errors.  The second argument is the data
  // value to be cleared if the IGNORE_CLEAR or WARN_CLEAR policy is used.
  // The logical dimensions of the fwrap arguments determine the domain
  // indices and function indices.  The single dimension of x and the last dimension
  // of f must agree when both are fwrap objects (first form of check()).  
  // The remaining dimensions of f are assumed to be function values at 
  // those indices.  If x is a single float (second form of check()), then 
  // all of f is considered to be function values.
  //
  template<typename FP> bool check(const fwrap<FP>& x, fwrap<FP>& f) const ;  // array of data
  template<typename FP> bool check(const FP& x,        fwrap<FP>& f) const ;  // single data point of array
  template<typename FP> bool check(const FP& x, FP& f)               const ;  // single data point of value

  //
  // ------ static helper -------
  //

  //
  // handy for indicating out of range errors in React objects
  //
  enum range_error {RANGE_OK, RANGE_CHECK} ;

  //
  // if the argument x is outside the xlow,xhigh range, set rerr=RANGE_CHECK
  // and put it in range.  rerr should be initialized to RANGE_OK before calling
  // this routine.
  //
  template<typename FP> static inline void range_check(FP& x, const FP& xlow, const FP& xhigh, range_error& rerr) {
    if (x<xlow) {
      x=xlow ; rerr = RANGE_CHECK ;
    }
    else if (x>xhigh) {
      x=xhigh ; rerr = RANGE_CHECK ;
    } ;
  } ; 

private:
  void tryswap() ;      // check the minimum and maximum and swap if inconsistent

  string label ;        // used in error messages

  scaling rscale ;      // describes the type of scale this range covers

  fp_64 minimum ;
  fp_64 maximum ;

  policy minimum_policy ;
  policy maximum_policy ;

  fp_64 clear_minimum ;       // value to use if exceed minimum with *_CLEAR policy
  fp_64 clear_maximum ;       // value to use if exceed maximum with *_CLEAR policy
  fp_64 clear_scale_minmax ;  // relative scale for the clear_* values (for state)
} ;

ostream& operator<<(ostream& s, Range::policy p) ;
ostream& operator<<(ostream& s, Range::scaling p) ;
ostream& operator<<(ostream& s, const Range& r) ;

//
// gcc 2.91 prefers it here (??)
//
template<typename FP> inline void range_check(FP& x, const FP& xlow, const FP& xhigh, Range::range_error& rerr) {
  if (x<xlow) {
    x=xlow ; rerr = Range::RANGE_CHECK ;
  }
  else if (x>xhigh) {
    x=xhigh ; rerr = Range::RANGE_CHECK ;
  } ;
} ; 

#include "range_temp.h"       // stupid compiler

//-----------------------------------------------------------------------------

#endif

