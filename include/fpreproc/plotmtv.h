/* -*-C++-*- */

// Interface object for the plotmtv program 
// A. Pletzer 24.6.98/28.10.99

#ifndef __plotmtv__
#define __plotmtv__

#include <string>
#include "Vector.h"
#include "Matrix.h"

const double plotmtv_ZERO_ONE[2] = {0., 1.};

/** Class interfacing to the plotmtv program. 
    Can do 2-d or simple 3-d plots. Arguments take typically
    either Vector or Matrix instances. Relies on 'system' command
    to launch plotmtv program (thus only runs on UNIX-like OS).
 */ 


class plotmtv {

private:

  /// randomly generated id number
  int plotid;

  public: 
 
  /// set this to true for multiplots
  bool hold;
  /// set this to true if file needs to be saved
  bool save;
  /// set this to true if new file is to be created
  bool freshstart;
  /// set this to true to to have plot displayed.
  bool display;
  /// labels
  std::string toplabel, xlabel, ylabel, zlabel, linelabel;
  /// varies fom 1-32
  int linewidth; 
  /// varies from 1-10
  int markersize; 
  /// varies from 1-30
  int fontsize;   
  
  /** Constructor. Takes care of opening file.
      @param id if set to zero, a radom file name will be generated. Otherwise
      the filename will bare the name "table<id>.mtv".
   */

    plotmtv(int id=0);    

  /*.................
    copy constructors 
    .................*/


  // none


  /** Destructor. Close file.
   */
    ~plotmtv();

  
  /** 2-D plot. 

      Colours can be k (black), y (yellow), c (cyan), g (green), r (red), b (blue), m (magenta), p (pink), s (steelblue), n (brown).
      Lines can be ' ' (none), | (solid), l (dashed), ! (dotted-dashed), : (dotted)
      Markers can be ' ' (none), . (dots), +, x, s (squares), l (lozenges), \^ (up-triangles), v (down-triangles), o
      
      @param x abscissae
      @param y ordinates
      @param line line colour/type. Examples are "r:" (red dotted), "b|" (blue solid), "gI" (green dotted)
      @param marker marker colour/type. Examples are "ro", "bx", "g."


  */
    void plot(const Vec &x,const Vec &y, 
	      std::string line="b|", std::string marker="ro");


		    /* 3-d plot */


  /** 3-D plot on regular/uniform mesh.
      @param z elevation
      @param xrange min/max along x (default is {0., 1.})
      @param yrange min/max along y (default is {0., 1.})
      @param zrange min/max along z (default is {0., 1.})
   */
    void plot(const Mat &z, 
	      const double xrange[2]=plotmtv_ZERO_ONE, 
	      const double yrange[2]=plotmtv_ZERO_ONE, 
	      const double zrange[2]=plotmtv_ZERO_ONE) ;

  /** 3-D plot on non-uniform, rectilinear mesh.
      @param x x coordinates
      @param y y coordinates
      @param z elevation
   */
    void plot(const Vec &x,const Vec &y,const Vec &z) ;

  /** 3-D plot on general, irregular mesh.
      @param x x coordinates
      @param y y coordinates
      @param z elevation
   */
    void plot(const Mat &x,const Mat &y,const Mat &z) ;


};

#endif /* __plotmtv__ */
