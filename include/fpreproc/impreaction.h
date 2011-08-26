#ifndef IMPREACTION_H
#define IMPREACTION_H

#include "react.h"

//
// ------------------- ImpReact_Rad ------------------
// adds an interface for manipualting brem... part of radiation reaction
//
// example:
//   ImpReaction r(8) ;
//   ImpReact_Rad::cast(r.rad())->set_brem(true) ;  // add brem.. to radiation
//
class ImpReact_Rad  {
public:
  virtual ~ImpReact_Rad() ;

  virtual bool brem() const     = 0 ; // true if including brem... in radiation 
  virtual void set_brem(bool b) = 0 ; // set true to include brem... in radiation power

  // --- static ---
  static ImpReact_Rad* cast(const React&) ;
} ;


//
// ------------------------ ImpReaction -----------------------
// Encapsulates the reactions associated with impurity rates
// Currently includes:
//    - electron collisional ionization rate (sec-1)
//    - recombination rate due to radiative and dielectronic
//      recombination (sec-1)
//    - radiation in (joules/sec) due to collisional excitation
//      and recombination and optionally brem...(default is no brem...)
//    - brem... radiation only in (joules/sec) 
//    - static functions for calculating coronal equilibrium
//
// For all four rates:
//    arguments:  0-> Te (keV)  electron temperature
//                1-> Ne (m-3)  electron density
//
//    result:     rank 1 with shape [Z+1]
//                [i]-> rate for charge state i,  i=0...Z
//
//    vector2(): should be used for multiple Te[j],Ne[j],result[i,j] 
//               evaluations where 
//                [j]-> index of reaction computation
//                [i]-> index for charge state,  i=0...Z
//
// Note: If brem... is set to be included in radiation rad() React, then
//       trying to call brem() will throw an exception.  This is only to
//       prevent double counting of brem... in total radiation.
//
// example:
//    ImpReaction r(6) ;          // build carbon
//    ap_32 xarg(2), result(7) ;
//    xarg(0) = 10. ;             // Te in keV
//    xarg(1) = 1.e19 ;           // Ne in m-3
//    r.ioniz()(xarg,result) ;    // fill result array
//
class ImpReaction {
private:
  void bcheck() const ;  // throw an exception if brem... is supposed to
                         // be included in rad() reaction
public:
  //
  // ---  constructor ---
  // The option value is used to select the objects used to implement
  // the impurity rates.
  //
  enum option {ADPAK_DIRECT=0} ;

  ImpReaction(int nucz, option r=ADPAK_DIRECT) ;
  ~ImpReaction() { } ;

  // --- reaction data ---
  int    nucz() const { return knucz ; } ;  // atomic number of this element
  string name() const { return kname ; } ;  // name of this reaction

  string info() const ;                    // informative

  //
  // -- modify the out of range policy for all three Reacts  --
  // Only the policy is modifiable here because the minimum and maximum
  // should be intrinsic to the implementation method.  The clear_minimum
  // and clear_maximum values are set to the default of 0.0 by these calls.  
  // Changing the policy could result in rebuilding an internally cached table.
  //   i=0 -> Te
  //    =1 -> Ne
  void r_set_min_policy(size_t i, Range::policy pmin) ;  // set policy for axis Range i
  void r_set_max_policy(size_t i, Range::policy pmax) ;

  // --- Reacts ---
  React ioniz() { return rioniz ; } ;             // Ionization    (sec-1)
  React recom() { return rrecom ; } ;             // Recombination (sec-1)
  React rad()   { return rrad ; };                // Radiation     (joules/sec)
  React brem()  { bcheck() ; return rbrem ; } ;   // Brem          (joules/sec)

  // --- static ---
  static string table_name(int knucz) ; // return name in the form  "Helium (He)"
  
  //
  // ----- static coronal equilibrium -----
  // These functions can operate in either single evaluation or vector
  // form depending on the rank of the input arrays
  //
  // coronal: calculates the coronal equilibrium given the ionization
  //          and recombination rate
  //          single mode,
  //             ioniz[Z+1] -> input -- rank 1 array of ionization rate over
  //                           charge states 0...Z
  //             recom[Z+1] -> input -- rank 1 array of recombination rate over
  //                           charge states
  //             coron[Z+1] -> output -- rank 1 array of fraction of particles in 
  //                           each charge state.  This is normalized
  //                           to a sum of 1.
  //          vector mode,
  //             ioniz[Z+1,N] -- input  -- same but rank 2 for N different sets of data
  //             recom[Z+1,N] -- input
  //             coron[Z+1,N] -- output
  //
  // coronal_rad: returns the total radiation in (joules/sec) based on the
  //              supplied coronal_equilibrium and radiation rate
  //          single mode,
  //             coron[Z+1] -> input -- rank 1 array of fraction of particles in 
  //                           each charge state as returned by coronal(), need not
  //                           be normalized to a sum of 1.
  //             rad[Z+1]   -> input -- rank 1 array of radiation rate in (joules/sec)
  //             total[]    -> output -- rank 0 array of total radiation in (joules/sec)
  //
  //          vector mode,
  //             coron[Z+1,N] -- input  -- same but rank 2 for N different sets of data
  //             rad[Z+1,N]   -- input  -- same but rank 2 for N different sets of data
  //             total[N]     -- output -- same but rank 1 for N different sets of data
  //
  template<typename FP> static void coronal(const fwrap<FP>& ioniz, const fwrap<FP>& recom, fwrap<FP>& coron) ;
  template<typename FP> static void coronal_rad(const fwrap<FP>& coron, const fwrap<FP>& rad, fwrap<FP>& total) ;

private:
  template<typename FP> static void coronal_ptr(size_t z1, const FP ioniz[], const FP recom[], FP coron[]) ;

  int    knucz ;  // nuclear Z used in construction
  string kname ;  // a descriptive name
  option kopt ;   // option used during construction

  React rioniz ;  // ImpReact for ionization
  React rrecom ;  // ImpReact for recombination
  React rrad ;    // ImpReact,ImpReact_Rad for radiation
  React rbrem ;   // ImpReact for brem...
} ;

ostream& operator<<(ostream& c, ImpReaction::option r) ;
ostream& operator<<(ostream& c, const ImpReaction&  r) ;

//
// -------------------- ImpReactionTable ---------------
// provides a simplified access to a set of Impurity Reactions
// as a singleton.  The ImpReactions will be built with the
// default build option of def_build() if they are not defined
// when Element(i) is called -- 'i' is the atomic number.
// Default is no brem... in radiation rate.
//
// usage:
//    ImpReactionTable::Element(6)->ioniz()(xarg,result) ;            // get ionization rate of carbon
//    ImpReactionTable::table()->set_def_option(ImpReaction::ADPAK_DIRECT) ; // change default build
//    ImpReactionTable::table()->erase(6) ;                           // remove old version
//    ImpReactionTable::Element(6)->ioniz()(xarg,result) ;            // get ionization rate of carbon
//                                                                    // using the new default
//    ImpReactionTable::Element(8)->ioniz()(xarg,result) ;            // get ionization rate of oxygen
//                                                                    // built using new default
class ImpReactionTable {
public:
  ~ImpReactionTable() ;

  // --- const ---
  static const size_t max_elements = 109 ;                           // good luck with Einsteinium

  typedef ImpReaction::option Ioption ;
  static const Ioption init_def_option = ImpReaction::ADPAK_DIRECT ; // initial default

  // --- object access ---
  ImpReaction* operator[](size_t i) ; // throws an exception if atomic number 'i'
                                      // is out of bounds

  // --- build ---
  Ioption def_option() const { return dopt ; } ;  // default build option
  bool    def_brem()   const { return dbrem ; } ; // true if brem... flag will be set on radiation
                                                  // react when building new ImpReactions

  void set_def_option(Ioption d) { dopt=d  ; } ;  // change the default build option
  void set_def_brem(bool b)      { dbrem=b ; } ;  // set true so brem.. will be included in radiation rates

  void build(size_t i, Ioption r, bool brem=false) ; // build or rebuild this element

  // --- delete ---
  void erase(size_t i) ;                          // delete the existing ImpReaction* for this element
  void clear() ;                                  // erase all the elements

  // --- singleton ---
  static ImpReactionTable* table() ;             // returns the singleton table
  static ImpReaction* Element(size_t i) { return table()->operator[](i) ; } ;  // for convenience

protected:
  ImpReactionTable() ;

private:
  ImpReactionTable(const ImpReactionTable&) ;            // no copying
  ImpReactionTable& operator=(const ImpReactionTable&) ; // no assignment
  
  static void check(size_t i) { if (i<1 || i>max_elements) 
    TRANSP_THROW("?preact::ImpReactionTable::check(): Atomic number is out of bounds") ; 
  } ;

  static ImpReactionTable* s ; // the singleton table
  // --- data ---
  Ioption dopt ;               // ImpReaction build option
  bool    dbrem ;              // true to set brem... flag on radiation rate

  vector <ImpReaction*> pr ;   // one for each element
} ;

#include "impreaction_temp.h"  // template definitions

#endif
