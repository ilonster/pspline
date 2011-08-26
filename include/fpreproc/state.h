#ifndef STATE_H
#define STATE_H

#include "iutil.h"
#include <vector>

#ifdef __GNUC__
#if __GNUC__ == 2
#define IOS_BASE ios
#else
#define IOS_BASE ios_base
#endif
#else
#define IOS_BASE  ios_base
#endif

//
// ------------------- doublescale ------------------
// a double is stored along with a typical scale for comparison with
// other doubles in the function isclose().  The output is written in
// high precision for the value and low precision for the scale as
// a [value,scale] pair.
//
class doublescale {
public:
  doublescale(fp_64 a=0., fp_64 b=1.0) : value(a), scale(b) { } ;
  fp_64 value ;
  fp_64 scale ;  // must be >0. -- can be approximate
} ;

const fp_64 close_epsilon = 1.e-7 ;   // epsilon for deciding closeness
                                      // relative to a scale

bool isclose(const doublescale&, const doublescale&) ;  // return true when values are close enough

ostream& operator<<(ostream& s, const doublescale& q) ; // modifies precision
istream& operator>>(istream& s, doublescale& q) ;

//
// -------------------- state ----------------------
// object used by other objects to store enough information
// to uniquely identify themselves -- the state can be read/written
// and states can be compared for equality.  Current implementation
// allows storage of an arbitrary number of strings, ints and
// doubles (with scales) and other states.  The internal states
// are copies, not references, to other states.  Each client object
// decides the meaning of the storage.
//
class state {
public:
  state() ;                           // create with empty lists
  state(const state&) ;               // copy constructor
  state& operator=(const state&) ;    // assignment
  ~state() ;
  
  bool operator==(const state& q) const ;  // compare states for equality

  // public list of states which are part of this state
  size_t vsize() const  ;             // size of state data
  void   vresize(size_t) ;            // change the state storage size
  state& vstate(size_t) ;             // full modification access
  const state& vstate(size_t) const ; // const version

  // public lists of atomic data describing the state
  vector<string>       vs ;           // no limit checks so be careful
  vector<int>          vi ;
  vector<doublescale>  vd ;

private:
  //
  // In order to break the creation recursion, the vector of states
  // is left as a null pointer until accessed through the public interface.
  // The state still has a value behaviour.  Current implementation always
  // has a null pointer when the state vector size is zero.
  //
  vector<state>* vp ;     
} ;

//
// i/o of state in ascii format but not too human readable
// format is currently
// < num_string num_int num_doublescale num_state string0 string1 ... int0 int1 ... doublescale0
//   doublescale1 ... <state0> <state1> ... >
//
ostream& operator<<(ostream&, const state&) ;
istream& operator>>(istream&, state&) ;

#endif
