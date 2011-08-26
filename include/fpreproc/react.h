#ifndef REACT_H
#define REACT_H

#include "iutil.h"
#include "state.h"
#include "range.h"
#include <list>

//
// These classes define the basic reaction interface.
//
// -- Reference Semantics --
// Reactions are represented by aReact and derived classes which
// are manipulated through the React smart pointer.  Static creation
// operators return new reactions in a React object.  The React
// object can be copied and assigned to create new references to the
// original reaction.  When the last React reference is deleted, the
// reaction will be deleted.
//

class React ;  // forward reference

//
// ------------------------- aReact ------------------------
// Actual reactions should derive from this class.  Derived classes have
// the following responsibilities:
// (1) Implement call(), vcall() ...etc.  The default is to throw an exception.
//     Only the vcalln() routines with the correct number of arguments should
//     be implemented along with the operator() methods.
//     These methods should use min(),max() to determine when the arguments
//     exceed the axis ranges and return RANGE_OK if all the arguments are within
//     range or RANGE_CHECK if they are not or were never checked.  The React
//     interface uses this info to decide whether to apply Range::check() on
//     the arguments.
//
// (2) Implement update().  This function should be lightweight to indicate
//     to this object that some dependency React has changed its state.  Normally
//     this would be used to set a flag that the cached data may be corrupt.
//
// (3) Implement current() to return a state object describing this aReact's state.
//     Derived classes should call the base class current() to include it in the 
//     final state.
//
// (4) Any public interface methods which modify this object's state must call
//     notify().  A good way to do this would be:
//         ...
//         public:
//            void public_change_state() { my_internal_change_state() ; notify() ; } ;
//         ...
//         protected:
//            virtual void my_internal_change_state() { whatever... } ;
//
//     Any modification to this method should happen through the virtual function.  This
//     will guarentee that notify() is called only once after the state has been finalized
//     at the end of the public method call.
//
// (5) Implement static constructors which return a React and a static cast method from a React
//     to your derived React type.  Somewhere in the construction the Range objects should
//     be initialized.
//
// (6) Implement dprint() to add derived class data to the output.
//
class aReact {
public:
  virtual ~aReact() ;

  // --- probe ---
  string name()    const {return fname ; } ; // descriptive name

  // --- arguments ---
  size_t nargs()   const { return nx ; } ;   // number of arguments of function

  fp_64  min(size_t i) const ;          // return the minimum of this axis range  0<=i<nargs()
  fp_64  max(size_t i) const ;          // return the maximum of this axis range  0<=i<nargs()

  const Range& range(size_t i) const ;  // non-modifiable Range object

  // --- result ---
  size_t frank()    const { return shape.size() ; } ;  // rank  of result for one function evaluation
  Shape  fshape()   const { return shape ; } ;         // shape of result for one function evaluation

  bool   isscalar() const { return (shape.size()==0) ; } ; // true if result is scalar

  // --- modify ---
  // any public interface methods of a derived class which change the object's 
  // state should call notify() which calls update() on this object and 
  // the React objects in the observer list.
  //
  void attach(const React&) ;   // add a React to the list which will be notified
  void detach(const React&) ;   // remove this React from the list
  void notify(unsigned cnt=0) ; // calls update() then notifies Reacts in lreact with incremented cnt

  // -- modify argument Ranges, calls notify()  --
  void set_scale(size_t i, Range::scaling s) ; // describe scale of argument i as LOG or LINEAR

  void set_min(size_t i, fp_64 min) ;  // set the minimum for argument i
  void set_max(size_t i, fp_64 max) ;  // set the maximum for argument i

  //
  // set min/max policy for Range argument i -- do not ignore clear_scale.
  // It should be set to the relative size of the clear_value or is not changed if <=0.
  // This is important for comparing the state of the Range object.  The same clear_scale 
  // is used for both the min and max policy.  The defaults are (IGNORE, clear_value=0., clear_scale=1.).
  //
  void set_min_policy(size_t i, Range::policy pmin, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;
  void set_max_policy(size_t i, Range::policy pmax, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;

  // --- recover base pointer ---
  static aReact* cast(const React&) ;  // return the aReact* corresponding to this React

protected:
  aReact(string name, size_t num_args, Shape fshape) ;

  Range& range(size_t i) ;  // to be used for initializing the Ranges without
                            // causing a notify() call like with the public interfaces

  // --- virtuals ---
  virtual void call(const ap_64& x, ap_64& f, Range::range_error& rerr) ;
  virtual void call(const ap_32& x, ap_32& f, Range::range_error& rerr) ;

  virtual void vcall1(const ap_64& x, ap_64& f, Range::range_error& rerr) ;
  virtual void vcall1(const ap_32& x, ap_32& f, Range::range_error& rerr) ;

  virtual void vcall2(const ap_64& x, const ap_64& y, ap_64& f, Range::range_error& rerr) ;
  virtual void vcall2(const ap_32& x, const ap_32& y, ap_32& f, Range::range_error& rerr) ;

  virtual void update() ;         // implementation for telling this object that it's dependency
                                  // state might have changed -- should be lightweight because 
                                  // it might get called alot

  virtual state current() const ; // return an appropriate state object

  virtual string dprint() const ; // descriptive output string of derived class specifics

  // --- dangerous ---
  fp_64 xmin(size_t i) const { return xrange[i].min() ; } ;  // unchecked argument ranges
  fp_64 xmax(size_t i) const { return xrange[i].max() ; } ;

private:
  friend class React ;

  aReact(const aReact&) ;              // no copying
  aReact& operator=(const aReact&) ;   // no assignment

  string print()  const ;  // descriptive multiline output string 


  // ----- data -----
  static const unsigned MAX_LINK=15 ;  // notifications will die when this limit is hit

  string           fname ;    // a descriptive name for this reaction value
  size_t           nx ;       // number of arguments for function
  Shape            shape ;    // shape of result (dim[0], dim[1], ..., dim[rank-1])

  vector<Range>    xrange ;   // describes the range of the axis
  list<React*>     lreact ;   // list of Reacts which will be notified when this React is notified

  unsigned         count ;    // reference count
} ;

ostream& operator<<(ostream& c, const React& r) ;

//
// ---------------------------- React ------------------------
// Describes a single function in a reaction.  This object has
// reference semantics so copying it will refer to the same 
// underlying reaction (aReact).  The function it describes has a fixed
// number of arguments, nargs(), and can return an array as
// a result of its invocation.
//
class React {
public:
  // --- object stuff ---
  React(aReact* q) ;                  // null pointer only allowed in these four operations
  React(const React& q) ;
  React& operator=(const React& q) ;
  ~React() ;

  bool operator==(const React& q) const { return (p==q.p) ; } ;  // reference comparison
  bool operator!=(const React& q) const { return (p!=q.p) ; } ;

  // --- arguments ---
  size_t nargs()       const { return p->nargs() ; } ;  // number of arguments of function

  fp_64  min(size_t i) const { return p->min(i) ; } ;   // return the minimum of this argument range  0<=i<nargs()
  fp_64  max(size_t i) const { return p->max(i) ; };    // return the maximum of this argument range  0<=i<nargs()

  const Range& range(size_t i) const { return p->range(i) ; } ;  // non-modifiable Range object

  // --- result ---
  size_t frank()    const { return p->frank() ; } ;    // rank  of result for one function evaluation
  Shape  fshape()   const { return p->fshape() ; } ;   // shape of result for one function evaluation

  bool   isscalar() const { return p->isscalar() ; } ; // true if result is scalar(rank=1, size(0)=1)

  // --- calling ---
  // evaluate the function once -- xargs is a 1d array containing all the arguments
  // so xargs.size(0)==nargs() is required.  The result is stored in f so f must
  // have the shape fshape().  The return logical is true if a WARN range error
  // occurred during evaluation.
  //
  bool operator()(const ap_64& xargs, ap_64& f) ; 
  bool operator()(const ap_32& xargs, ap_32& f) ;

  //
  // vector calls -- use vector1() if nargs()==1, vector2() if nargs()==2 ...
  //
  // Each argument (x,y,...) must be a 1 dimensional array with logical
  // length equal to the number of function evaluations.  The result is put
  // in the array fv which must have rank fv.rank()=frank+1.  The shape
  // of fv = (fshape()[0], fshape()[1], ..., fshape()[frank-1], x.size(0))
  // In other words, fv has the shape of the function result plus an extra
  // dimension which is the size of the number of function evaluations = x.size(0).
  // This is the way that you would make this call in fortran except the
  // logical and physical size of the arrays are built into the ap_64,ap_32 objects.
  //
  // A logical true is returned if a WARN range error occurred.
  //
  // skip_arg_check=true will skip checking the ranks and shapes of the arguments.
  //
  bool vector1(const ap_64& x, ap_64& fv, bool skip_arg_check=false) ;
  bool vector1(const ap_32& x, ap_32& fv, bool skip_arg_check=false) ;

  bool vector2(const ap_64& x, const ap_64& y, ap_64& fv, bool skip_arg_check=false) ;
  bool vector2(const ap_32& x, const ap_32& y, ap_32& fv, bool skip_arg_check=false) ;

  //
  // set min/max policy for Range argument i -- do not ignore clear_scale.
  // It should be set to the relative size of the clear_value or is not changed if <=0.
  // This is important for comparing the state of the Range object.  The same clear_scale 
  // is used for both the min and max policy.  The defaults are (IGNORE, clear_value=0., clear_scale=1.).
  //
  void set_min_policy(size_t i, Range::policy pmin, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;
  void set_max_policy(size_t i, Range::policy pmax, fp_64 clear_value=0., fp_64 clear_scale=-1.) ;

  //
  // --- observers ---
  // There may be React objects which need to be notified when the state
  // of this object changes.  The following functions implement this observer behaviour.
  //
  void attach(React r) { p->attach(r) ; } ; // add a React to the list which will be notified
  void detach(React r) { p->detach(r) ; } ; // remove this React from the list
  void notify()        { p->notify() ; } ;  // force a notify event on each of the Reacts in the observer list

  // --- state ---
  string name()    const {return p->name() ; } ;     // name of this reaction result
  state  current() const {return p->current() ; } ;  // return an appropriate state object
  
  // --- testing ---
  static React create(string name, size_t num, Shape shape) {
    return React(new aReact(name,num,shape)) ;
  } ;
private:
  friend class aReact ;
  friend ostream& operator<<(ostream& c, const React& r) ;

  string print() const {return p->print() ; } ;      // return a multiline descriptive string

  aReact* p ;  // guarenteed not null
} ;

//
// -------------------- tReact -----------------------
// for testing, implements a 2 argument rank 1,size 3 function  
//   f(x,y) = [x+y,x-y,x*y]
// 
// x arg: log,    1<x<10, min_policy=IGNORE_CLEAR (2), max_policy=WARN_CLEAR (47)
// y arg: linear, 2<x<8,  min_policy=WARN,             max_policy=HALT
//
class tReact : public aReact {
public:
  virtual ~tReact() ;

  static React create(string name) ; // public constructor of tReacts
protected:
  tReact(string name, size_t num, Shape fshape) ;

  // --- virtuals ---
  virtual void call(const ap_32& x, ap_32& f, Range::range_error& rerr) ;
  virtual void call(const ap_64& x, ap_64& f, Range::range_error& rerr) ;

  virtual void vcall2(const ap_32& x, const ap_32& y, ap_32& f, Range::range_error& rerr) ;
  virtual void vcall2(const ap_64& x, const ap_64& y, ap_64& f, Range::range_error& rerr) ;

  virtual void update() ;         // implementation for telling this object that it's dependency
                                  // state might have changed -- should be lightweight because 
                                  // it might get called alot

  virtual state current() const ; // return an appropriate state object

  virtual string dprint() const ; // descriptive output string of derived class specifics

private:

} ;

#endif 
