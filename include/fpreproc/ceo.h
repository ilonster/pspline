#ifndef __ceo__
#define __ceo__

#include <math.h>
#include <iostream>
#include <string>
#include <time.h>
#include "Vector.h"
#include "Matrix.h"
#include "plotmtv.h"
#include "cubspline.h"
#include "deriv2d.h"
#include "EZcdf.hh"
#include "const.h"

using namespace std;

typedef double Real;
typedef ColMat<Real> Mat;
typedef Vector<Real> Vec;

#include "jsolver_names.h"
extern "C" void jsointerp2d_(unsigned int *ntjso,unsigned int *nsjso,
			     Real *tjso,Real *sjso,Real *rjso,
			     unsigned int *nt   ,unsigned int *ns   ,
			     Real *t   ,Real *s   ,Real *r    );


const std::string CEO_ALL = "all";
char CEO_SOLID_BLUE[3]="b|"; // default colour/line type
char CEO_MARKER[3]="  "; // default marker type

extern string today();

/*++++++++++++
 + class ceo +
 ++++++++++++*/

class ceo {

 public:
  std::string title;
  std::string label;
  bool ifail;
  int ierror;      // =0 if no error, != otherwise
  unsigned int ns; // # of radial surfaces
  unsigned int nt; // # number of poloidal sections
  Real rmag, zmag; // magnetic axis position
  Vec s  ;         // radial coordinate
  Vec t  ;         // poloidal coordinate
  Vec psi;         // poloidal flux in Wb
  Vec psibar;      // = psi/TwoPi
  Vec phi;         // toroidal flux in Wb
  Vec phibar;      // = phi/TwoPi
  Vec g  ;         // Btor * R
  Vec iota;        // rotational transform
  Vec p  ;         // pressure /mu0
  Vec ra, za;      // Plasma-vacuum boundary points
  Mat r, z;        // mesh: 1st (fast) index is poloidal 2nd (slow) index is radial

  /*...........
    constructor 
    ...........*/

  ceo(){ 
  ierror = 0;
  ns = 65;
  nt = 64;
  ifail = false;
  };


  /*................
    copy constructor 
    ................*/

  //ceo(ceo &old){}; // default constructor ok

  /*..........
    destructor 
    ..........*/

  //virtual ~ceo();
  
  //void run(Vec &ra_in, Vec &za_in, Vec &psi_in, Vec &pp_in, Vec &jb_in);

  /*.......
    methods
    .......*/

   Real R0(void);
   Real B0(void);
   Real area(void);
   Real elongation(void);
   Real minorRadius(void);
   Real volumeAveragedPressure(void);
   Real plasmaCurrent(void);
   Real betaToroidal(void); // in %
   Real betaPoloidal(void);
   Real beta(void);
   Real qStar(void);
   
   Vec vPrime(void);
   Vec get_psi(void);
   Vec get_psibar(void);
   Vec get_phi(void);
   Vec get_phibar(void);
   Vec get_g(void);
   Vec get_iota(void);
   Vec get_p(void);
   Mat get_r(Vec &t);
   Mat get_z(Vec &t);

   void plot(char *quant_c, char *line, char *symbol, 
	     bool display, bool freshstart);
   void plot(std::string quant=CEO_ALL, char *line=CEO_SOLID_BLUE, char *symbol=CEO_MARKER, 
	     bool display=true, bool freshstart=false);
   void load(std::string filename);
   void save(std::string filename);
   void loadCheaseFormat(std::string filename);
   void saveCheaseFormat(std::string filename);
   void saveRzsolverFormat(std::string filename);
   void toOrbit(std::string filename);
   void loadInput(std::string filename);

   Mat r_jphi(void);
   Mat d_dz(Mat &f);
   Mat d_dr(Mat &f);
   Mat Laplacian_star(void);
   Mat gradPsibarSq(void);
   Mat gradThetaSq(void);
   Mat gradPsibarGradTheta(void);
   Mat nDotGradPsibar(void);
   Mat BSq(void);
   Mat jacobian2D(void);               // poloidal Jacobian;
   Mat jacobian(void);                 // 3-D Jacobian
   Mat diffArcLength(void);            // d l /d theta
   Vec poloidalIntegral(const Mat &in);// poloidal integral 
                                       //with Jacobian weight
   Vec poloidalAverage(const Mat &in);
   Vec r_jParallel(void);      // <j.B>/<g/R^2>
   Vec plasmaCurrentDensity(void);     // d Ip /d psibar (using p', q and g')
   Vec plasmaCurrentDensity2(void);     // d Ip /d psibar (using p', g and g)
   void radialRemap(int i); // remap onto new radial uniform in psi^(i/2)
   void poloidalRemap(int m, int n, int l); // remap onto new grid with specified 
					    // Jacobian dependence
   Real GSRelError(void);



};
  
void ceo::loadCheaseFormat(std::string filename){

  /* read INP1 file in netcdf format */
  
  EZcdf file(filename, (std::string)"r");
  
  // Define variables

  
  std::string xtype;
  int dimlens[3];  

  file.cdfInqVar((std::string)"nxx", dimlens, xtype);
  int *nxx = new int[dimlens[2]];
  file.cdfGetVar((std::string)"nxx", &nxx[0]);
  ceo::nt = (unsigned int)nxx[0];
  ceo::ns = (unsigned int)nxx[1];
  if(nxx[2] != 0) {
    cerr << "ERROR ceo::loadCheaseFormat(std::string filename) isym=" << nxx[2] << 
      " not supported\n";
    ifail = true;
    ierror = 1;
  }
  delete[] nxx;
  
  if(t.size() == 0) t.alloc(nt+1);
  t.space(0.0, TwoPi); // assume uniform mesh in theta

  if(psibar.size() == 0) psibar.alloc(ns);
  file.cdfGetVar((std::string)"psival", &psibar(0));
  if(s.size() == 0) s.alloc(ns);
  ceo::s = psibar;
  ceo::s -= psibar(0);
  ceo::s /= (psibar(ns-1)-psibar(0));
  ceo::psi = TwoPi*psibar;

  if(iota.size() == 0) iota.alloc(ns);
  Vec q(ns);
  file.cdfGetVar((std::string)"q", &q(0));
  iota = 1.0/q;
  
  cubspline qint(psibar, q);
  ceo::phibar = qint.integrate(psibar);
  ceo::phi = TwoPi*ceo::phibar;

  Vec prof(ns);
  file.cdfGetVar((std::string)"pp", &prof(0));
  cubspline ppint(psibar, prof);
  ceo::p = ppint.integrate(psibar);
  ceo::p -= p(ns-1);

  //if(g.size() == 0) g.alloc(ns);
  //file.cdfGetVar((std::string)"g", &g(0));
  Vec gp(ns);
  file.cdfGetVar((std::string)"gp", &gp(0));
  cubspline gpint(psibar, gp);
  ceo::g = gpint.integrate(psibar);
  ceo::g += 1.0 - ceo::g(ns-1); // set the const

  if(r.size(0)*r.size(1) == 0) r.alloc(nt+1,ns);
  if(z.size(0)*z.size(1) == 0) z.alloc(nt+1,ns);
  double *twod = new double[(nt+5)*ns];

  file.cdfGetVar((std::string)"x", &twod[0]);
  for(unsigned int i=0; i<nt+1; i++){
    for(unsigned int j=0; j<ns; j++) r(i,j) = twod[j*(nt+5) + i + 2];
  }
  
  file.cdfGetVar((std::string)"z", &twod[0]);
  for(unsigned int i=0; i<nt+1; i++){
    for(unsigned int j=0; j<ns; j++) z(i,j) = twod[j*(nt+5) + i + 2];
  }

  delete[] twod;

  ceo::title = "Chease format";
  ceo::label = filename;
  ceo::rmag = r(0,0);
  ceo::zmag = z(0,0);
}


void ceo::saveCheaseFormat(std::string filename){

  /* save in INP1-netcdf file format */
  
  EZcdf file(filename, (std::string)"w");
  
  // Define variables

  
  std::string xtype_c = "CHAR";
  std::string xtype_i = "INT";
  std::string xtype_d = "R8";

  int dimlens[] = {1, 1, 1};

  file.cdfDefVar((std::string)"r0exp", dimlens, xtype_d);
  file.cdfDefVar((std::string)"b0exp", dimlens, xtype_d);
  
  dimlens[2] = title.length();
  file.cdfDefVar((std::string)"title", dimlens, xtype_c);
  dimlens[2] = label.length();
  file.cdfDefVar((std::string)"label", dimlens, xtype_c);
  dimlens[2] = 3;
  file.cdfDefVar((std::string)"nxx", dimlens, xtype_i);
  dimlens[2] = 5;
  file.cdfDefVar((std::string)"axx", dimlens, xtype_d);
  dimlens[2] = ns-1;
  file.cdfDefVar((std::string)"pm", dimlens, xtype_d);
  dimlens[2] = ns;
  file.cdfDefVar((std::string)"pp", dimlens, xtype_d);
  file.cdfDefVar((std::string)"q", dimlens, xtype_d);
  file.cdfDefVar((std::string)"qp", dimlens, xtype_d);
  file.cdfDefVar((std::string)"g", dimlens, xtype_d);
  file.cdfDefVar((std::string)"gp", dimlens, xtype_d);
  file.cdfDefVar((std::string)"fb", dimlens, xtype_d);
  file.cdfDefVar((std::string)"fbp", dimlens, xtype_d);
  file.cdfDefVar((std::string)"psival", dimlens, xtype_d);
  dimlens[2] = ns-1;
  file.cdfDefVar((std::string)"psivalm", dimlens, xtype_d);
  dimlens[1] = ns;
  dimlens[2] = nt+3; /* nt+5; */
  file.cdfDefVar((std::string)"x", dimlens, xtype_d);
  file.cdfDefVar((std::string)"z", dimlens, xtype_d);
  file.cdfDefVar((std::string)"aj3", dimlens, xtype_d);
  file.cdfDefVar((std::string)"aj", dimlens, xtype_d);

  
  double r0exp = R0(), b0exp = B0();
  file.cdfPutVar((std::string)"r0exp", &r0exp);
  file.cdfPutVar((std::string)"b0exp", &b0exp);

  char *title_c = (char *)title.c_str();
  file.cdfPutVar((std::string)"title", title_c);
  char *label_c = (char *)label.c_str();
  file.cdfPutVar((std::string)"label", label_c);
  int nxx[] = {nt, ns, 0};
  file.cdfPutVar((std::string)"nxx", &nxx[0]);
  double axx[] = {TwoPi/double(nt), 0.0, rmag, zmag, 1.0};
  file.cdfPutVar((std::string)"axx", &axx[0]);
  
  Vec psibm = ceo::psibar;
  for(unsigned int i=0; i<ns-1; i++) psibm(i) += (psibar(i+1)-psibar(i))/2.0;

  cubspline pint(ceo::psibar, ceo::p);
  Vec prof = pint.cubic(psibm);
  file.cdfPutVar((std::string)"pm", &prof(0));
  prof = pint.prime(psibar);
  file.cdfPutVar((std::string)"pp", &prof(0));

  Vec q = 1.0/ceo::iota;
  file.cdfPutVar((std::string)"q", &q(0));
  cubspline qint(psibar, q);
  Vec qp = qint.prime(psibar);
  file.cdfPutVar((std::string)"qp", &qp(0));

  file.cdfPutVar((std::string)"g", &g(0));
  cubspline gint(psibar, g);
  Vec gp = gint.prime(psibar);
  file.cdfPutVar((std::string)"gp", &gp(0));

  prof = g/q;
  file.cdfPutVar((std::string)"fb", &prof(0));
  prof = gp/q;
  prof -= g*qp/(q*q);
  file.cdfPutVar((std::string)"fbp", &prof(0));

  file.cdfPutVar((std::string)"psival", &psibar(0));
  file.cdfPutVar((std::string)"psivalm", &psibm(0));

  /* the following arrays have dimensions (nt+3, ns) */

  double *twod = new double[(nt+3)*ns];

  for(unsigned int j=0; j<ns; j++){
    for(unsigned int i=0; i<nt+1; i++) twod[j*(nt+3)+i] = r(i,j);
    twod[j*(nt+3)+nt+1] = twod[j*(nt+3)+1];
    twod[j*(nt+3)+nt+2] = twod[j*(nt+3)+2];
  }
  file.cdfPutVar((std::string)"x", &twod[0]);

  for(unsigned int j=0; j<ns; j++){
    for(unsigned int i=0; i<nt+1; i++) twod[j*(nt+3)+i] = z(i,j);
    twod[j*(nt+3)+nt+1] = twod[j*(nt+3)+1];
    twod[j*(nt+3)+nt+2] = twod[j*(nt+3)+2];
  }
  file.cdfPutVar((std::string)"z", &twod[0]);

  Mat jac = ceo::jacobian();
  for(unsigned int j=0; j<ns; j++){
    for(unsigned int i=0; i<nt+1; i++) twod[j*(nt+3)+i] = jac(i,j);
    twod[j*(nt+3)+nt+1] = twod[j*(nt+3)+1];
    twod[j*(nt+3)+nt+2] = twod[j*(nt+3)+2];
  }
  file.cdfPutVar((std::string)"aj", &twod[0]);

  for(unsigned int j=0; j<ns; j++){
    for(unsigned int i=0; i<nt; i++){ 
      twod[j*(nt+3)+i] += (jac(i+1,j)-jac(i,j))/2.0;
    }
    twod[j*(nt+3)+nt  ] = twod[j*(nt+3)  ];
    twod[j*(nt+3)+nt+1] = twod[j*(nt+3)+1];
    twod[j*(nt+3)+nt+2] = twod[j*(nt+3)+2];
  }
  file.cdfPutVar((std::string)"aj3", &twod[0]);  

  delete[] twod;

}

void ceo::saveRzsolverFormat(std::string filename){

  /* save data in rzsolver format */

  EZcdf file(filename, (std::string)"w");
  
  // Define variables

  std::string xtype_c = "CHAR";
  std::string xtype_d = "R8";
  int ns = psibar.size();
  int na = ra.size();

  int dimlens[] = {1, 1, 1};
  std::string comment = "CEO";
  dimlens[2] = comment.length();

  file.cdfDefVar((std::string)"comment", dimlens, xtype_c);
  dimlens[2] = na;
  file.cdfDefVar((std::string)"xbound", dimlens, xtype_d);
  file.cdfDefVar((std::string)"zbound", dimlens, xtype_d);
  dimlens[2] = ns;
  file.cdfDefVar((std::string)"psibar", dimlens, xtype_d);
  file.cdfDefVar((std::string)"pprime_of_psibar", dimlens, xtype_d);
  file.cdfDefVar((std::string)"psecond_of_psibar", dimlens, xtype_d);
  file.cdfDefVar((std::string)"g_of_psibar", dimlens, xtype_d);
  file.cdfDefVar((std::string)"gprime_of_psibar", dimlens, xtype_d);
  file.cdfDefVar((std::string)"gsecond_of_psibar", dimlens, xtype_d);

  file.cdfPutVar((std::string)"comment", (char *)comment.c_str());
  file.cdfPutVar((std::string)"xbound", &ra[0]);
  file.cdfPutVar((std::string)"zbound", &za[0]);
  file.cdfPutVar((std::string)"psibar", &psibar[0]);
  cubspline p_spline(ceo::psibar, p);
  cubspline g_spline(ceo::psibar, g);
  file.cdfPutVar((std::string)"pprime_of_psibar", &p_spline.prime(psibar)[0]);
  file.cdfPutVar((std::string)"psecond_of_psibar", &p_spline.second(psibar)[0]);
  file.cdfPutVar((std::string)"g_of_psibar", &ceo::g[0]);
  file.cdfPutVar((std::string)"gprime_of_psibar", &g_spline.prime(psibar)[0]);
  file.cdfPutVar((std::string)"gsecond_of_psibar", &g_spline.second(psibar)[0]);

}


void ceo::load(std::string filename){

  /* load object from netcdf file */
  
  EZcdf file(filename, (std::string)"r");
  
  // Define variables

  
  std::string xtype;
  int dimlens[3];  

  file.cdfInqVar((std::string)"title", dimlens, xtype);
  char *title_c = new char[dimlens[2]];
  file.cdfGetVar((std::string)"title", title_c);
  title = title_c;
  delete[] title_c;
  file.cdfInqVar((std::string)"label", dimlens, xtype);
  char *label_c = new char[dimlens[2]];
  file.cdfGetVar((std::string)"label", label_c);
  label = label_c;
  delete[] label_c;
  int snt;
  file.cdfGetVar((std::string)"nt", &snt);
  nt = (unsigned int)snt;
  int sns;
  file.cdfGetVar((std::string)"ns", &sns);
  ns = (unsigned int)sns;
  file.cdfGetVar((std::string)"rmag", &rmag);
  file.cdfGetVar((std::string)"zmag", &zmag);
  if(s.size() == 0) s.alloc(ns);
  file.cdfGetVar((std::string)"s", &s(0));
  if(t.size() == 0) t.alloc(nt+1);
  file.cdfGetVar((std::string)"t", &t(0));
  if(psi.size() == 0) psi.alloc(ns);
  file.cdfGetVar((std::string)"psi", &psi(0));
  if(psibar.size() == 0) psibar.alloc(ns);
  file.cdfGetVar((std::string)"psibar", &psibar(0));
  if(phi.size() == 0) phi.alloc(ns);
  file.cdfGetVar((std::string)"phi", &phi(0));
  if(phibar.size() == 0) phibar.alloc(ns);
  file.cdfGetVar((std::string)"phibar", &phibar(0));
  if(g.size() == 0) g.alloc(ns);
  file.cdfGetVar((std::string)"g", &g(0));
  if(iota.size() == 0) iota.alloc(ns);
  file.cdfGetVar((std::string)"iota", &iota(0));
  if(p.size() == 0) p.alloc(ns);
  file.cdfGetVar((std::string)"p", &p(0));
  //file.cdfGetVar((std::string)"ra", &ra(0));
  //file.cdfGetVar((std::string)"za", &za(0));
  if(r.size(0)*r.size(1) == 0) r.alloc(nt+1, ns);
  file.cdfGetVar((std::string)"r", &r(0,0));
  if(z.size(0)*z.size(1) == 0) z.alloc(nt+1, ns);
  file.cdfGetVar((std::string)"z", &z(0,0));
}  

void ceo::save(std::string filename){

  /* save object in netcdf file format */
  
  EZcdf file(filename, (std::string)"w");
  
  // Define variables

  std::string xtype_c = "CHAR";
  std::string xtype_i = "INT";
  std::string xtype_d = "DOUBLE";
  int dimlens_1[] = {1, 1, 1};
  int dimlens_ns[] = {1, 1, ns};
  int dimlens_nt[] = {1, 1, nt+1};
  int dimlens_nt_ns[] = {1, ns, nt+1};
  
  int dimlens_c[] = {1, 1, (int)(title.length()+1) };
  file.cdfDefVar((std::string)"title", dimlens_c, xtype_c);
  dimlens_c[2] = (int)(label.length()+1);
  file.cdfDefVar((std::string)"label", dimlens_c, xtype_c);
  file.cdfDefVar((std::string)"nt", dimlens_1, xtype_i);
  file.cdfDefVar((std::string)"ns", dimlens_1, xtype_i);
  file.cdfDefVar((std::string)"rmag", dimlens_1, xtype_d);
  file.cdfDefVar((std::string)"zmag", dimlens_1, xtype_d);
  file.cdfDefVar((std::string)"s", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"t", dimlens_nt, xtype_d);
  file.cdfDefVar((std::string)"psi", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"psibar", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"phi", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"phibar", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"g", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"iota", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"p", dimlens_ns, xtype_d);
  //file.cdfDefVar((std::string)"ra", dimlens_nt, xtype_d);
  //file.cdfDefVar((std::string)"za", dimlens_nt, xtype_d);
  file.cdfDefVar((std::string)"r", dimlens_nt_ns, xtype_d);
  file.cdfDefVar((std::string)"z", dimlens_nt_ns, xtype_d);

  char *title_c = (char *)title.c_str();
  char *label_c = (char *)label.c_str();
  int snt = (int)nt;
  int sns = (int)ns;
  file.cdfPutVar((std::string)"title", title_c);
  file.cdfPutVar((std::string)"label", label_c);
  file.cdfPutVar((std::string)"nt", &snt);
  file.cdfPutVar((std::string)"ns", &sns);
  file.cdfPutVar((std::string)"rmag", &rmag);
  file.cdfPutVar((std::string)"zmag", &zmag);
  file.cdfPutVar((std::string)"s", &s(0));
  file.cdfPutVar((std::string)"t", &t(0));
  file.cdfPutVar((std::string)"psi", &psi(0));
  file.cdfPutVar((std::string)"psibar", &psibar(0));
  file.cdfPutVar((std::string)"phi", &phi(0));
  file.cdfPutVar((std::string)"phibar", &phibar(0));
  file.cdfPutVar((std::string)"g", &g(0));
  file.cdfPutVar((std::string)"iota", &iota(0));
  file.cdfPutVar((std::string)"p", &p(0));
  //file.cdfPutVar((std::string)"ra", &ra(0));
  //file.cdfPutVar((std::string)"za", &za(0));
  file.cdfPutVar((std::string)"r", &r(0,0));
  file.cdfPutVar((std::string)"z", &z(0,0));
}  
  void ceo::plot(const std::string quant, char line[3], 
		 char symbol[3],
		 bool display, bool freshstart){

    //std::string quant(quant_c);
    plotmtv P((int)1); 
    P.save = true; // will save file
    P.display = display;
    P.freshstart = freshstart;
    P.toplabel = title;
    P.linelabel = label;
    P.ylabel = quant;
    P.xlabel = "psibar";
    if( quant == "p" || quant == "pressure" ){
	 P.plot(psibar, p, line, symbol);
    }
    else if( quant == "q" ){
	 P.plot(psibar, 1./iota, line, symbol);
    }
    else if( quant == "phi" ){
	 P.plot(psibar, phi, line, symbol);
    }
    else if( quant == "g" || quant == "G" || quant == "F" ){
	 P.plot(psibar, g, line, symbol);
    }
    else if( quant == "j.B" || quant == "r_jParallel" || quant == "<j.B>/<B.grad phi>" ){
	 P.plot(psibar, r_jParallel(), line, symbol);
    }
    else if( quant == "Ip"){
      Vec dip1 = plasmaCurrentDensity();
      Vec dip2 = plasmaCurrentDensity2();
      cubspline dip1_spl(psibar, dip1);
      cubspline dip2_spl(psibar, dip2);
      Vec ip1 = dip1_spl.integrate(psibar);
      Vec ip2 = dip2_spl.integrate(psibar);
      P.plot(psibar, ip1, line, symbol);
      P.hold = true;
      P.plot(psibar, ip2, "rl", "ko");
      P.hold = false;
    }
    else if( quant == "grid" || quant == "Grid"){
      Vec rflat = flatten0(r);
      Vec zflat = flatten0(z);
      P.xlabel  = "R";
      P.ylabel  = "Z";
      P.display = false;
      P.plot(rflat,zflat, line, "g+");
      P.hold = true;
      P.plot(ra, za, "rl", "ko");
      P.hold = false;
    }
    else if(quant == "jacobian" || quant == "Jacobian"  || quant == "jac"){
      Mat jac = jacobian();
      P.toplabel = "Jacobian";
      P.xlabel = "R";
      P.ylabel = "Z";
      P.zlabel = "J";
      int nsize = (ns-1)*nt;
      Vec x1(nsize), y1(nsize), z1(nsize);
      unsigned int k=0;
      for(unsigned int j=1; j<ns; j++) {
	for(unsigned int i=0; i<nt; i++){
	  x1(k) = r(i,j);
	  y1(k) = z(i,j);
	  z1(k) = jac(i,j);
	  k++;
	}
      }
      P.plot(x1, y1, z1);
    }
    else if(quant == "r jphi" || quant == "r j_phi"  || quant == "r j"){
      Mat rj = r_jphi();
      P.toplabel = "R J_phi";
      P.xlabel = "R";
      P.ylabel = "Z";
      P.zlabel = "R J_phi";
      int nsize = (ns-1)*nt;
      Vec x1(nsize), y1(nsize), z1(nsize);
      unsigned int k=0;
      for(unsigned int j=1; j<ns; j++) {
	for(unsigned int i=0; i<nt; i++){
	  x1(k) = r(i,j);
	  y1(k) = z(i,j);
	  z1(k) = rj(i,j);
	  k++;
	}
      }
      P.plot(x1, y1, z1);
    }
    else if(quant == "error" || quant == "Error"){
      Mat error = r_jphi();
      Real maxrj = max( error );
      error -= Laplacian_star();
      error /= maxrj;
      P.toplabel = "rel. error";
      P.xlabel = "R";
      P.ylabel = "Z";
      P.zlabel = "Error";
      int nsize = (ns-1)*nt;
      Vec x1(nsize), y1(nsize), z1(nsize);
      unsigned int k=0;
      for(unsigned int j=1; j<ns; j++) {
	for(unsigned int i=0; i<nt; i++){
	  x1(k) = r(i,j);
	  y1(k) = z(i,j);
	  //z1(k) = error(i,j);
	  z1(k) = min(fabs(error(i,j)), 1.0);
	  k++;
	}
      }
      
      P.plot(x1, y1, z1);
    }
    else {
      bool show=true, fresh=true;
      bool dont_show=false;
      display = dont_show;
      freshstart = fresh;
      plot((std::string)"p", line, symbol, dont_show, fresh);
      plot((std::string)"q", line, symbol, dont_show);
      plot((std::string)"g", line, symbol, dont_show);
      plot((std::string)"j.B", line, symbol, dont_show);
      plot((std::string)"phi", line, symbol, dont_show);
      plot((std::string)"Ip", line, symbol, dont_show);
      plot((std::string)"grid", line, symbol, dont_show);
      plot((std::string)"jac", line, symbol, dont_show);
      plot((std::string)"r j_phi",line, symbol, dont_show);
      plot((std::string)"error",line, symbol);
    }
}


Real ceo::R0(void){

  /* geometric R-centre */

  Real rmax = max(r);
  Real rmin = min(r);
  return (rmax+rmin)/2.0;
}

Real ceo::B0(void){

  /* vacuum toroidal magnetic field at R0 */

  return g(ns-1)/R0();
}

Real ceo::minorRadius(void){

  /* a */

  Real rmax = max(r);
  Real rmin = min(r);
  return (rmax-rmin)/(rmax+rmin);
}

Real ceo::area(void){
  
  /* poloidal area */ 

  Mat inverseR = 1.0/r;
  Vec line = poloidalIntegral(inverseR);
  cubspline density(psibar, line);
  Vec res = density.integrate(psibar);
  return res(ns-1);
}

Real ceo::elongation(void){
  
  /* kappa */

  Real aSq = minorRadius();
  aSq *= aSq;

  return area()/(Pi*aSq);
}


Vec ceo::vPrime(void){

  /* V' */

   Mat one(nt+1,ns,1.0);
   Vec vp = poloidalIntegral(one);
   vp *= TwoPi;
   return vp;
}

Real ceo::volumeAveragedPressure(void){

  /* <p> */

  Vec vP = vPrime(); 
  vP(0) = vP(1); // extrapolate to axis
  Vec pVp =  p*vP;

  cubspline intpVp(psibar, pVp);
  Vec num = intpVp.integrate(psibar);

  cubspline ointvP(psibar, vP);
  Vec den = ointvP.integrate(psibar);

  return num(ns-1)/den(ns-1);
}

Vec ceo::plasmaCurrentDensity2(void){

  /* d Ip / d psibar */
  // this version appears to be more accurate

  Mat one(nt+1,ns,1.0);
  cubspline p_spl(psibar, ceo::p);
  cubspline g_spl(psibar, ceo::g);
  Vec pp = p_spl.prime(psibar);
  Vec gp = g_spl.prime(psibar);
  return -pp*poloidalIntegral(one) - gp*TwoPi/ceo::iota;
}

Vec ceo::plasmaCurrentDensity(void){

  /* d Ip / d psibar */

  Mat rSq = r;
  rSq *= r;
  return poloidalIntegral( r_jphi()/rSq );
}

Real ceo::plasmaCurrent(void){

  /* Ip */

  cubspline density(psibar, plasmaCurrentDensity());
  return density.integrate(psibar)(ns-1);
}

  

Real ceo::betaToroidal(void){

  /* volume averaged toroidal beta = 2 <p>/B0^2  in % */

  Real bSq = B0();
  bSq *= bSq;
  return 100.0* 2.0*volumeAveragedPressure()/bSq;
}

Real ceo::betaPoloidal(void){

  /* 2 <p> / Bp^2 */ 

  Real BpSq = plasmaCurrent()/(TwoPi*minorRadius()*elongation());
  BpSq *= BpSq;

  return 2.0*volumeAveragedPressure()/BpSq;
}

Real ceo::beta(void){

  /* 2 <p> / B^2 */


  Real bSq = B0();
  bSq *= bSq;

  Real BpSq = plasmaCurrent()/(TwoPi*minorRadius()*elongation());
  BpSq *= BpSq;

  return 100.0 *2.0*volumeAveragedPressure()/(bSq + BpSq);
}

Real ceo::qStar(void){
  
  /* q* */

  Real aSq = minorRadius();
  aSq *= aSq;
  
  return TwoPi*aSq*elongation()*B0()/(R0()*plasmaCurrent());
}

Mat ceo::r_jphi(void){
  /*
    r J_phi or right-hand side of Grad-Shafranov equation
    del^2* psi = - r^2 p' - g g'
    */

  cubspline pres(psibar, p);
  Vec p_prime = pres.prime(psibar);
  cubspline gee(psibar, g);
  Vec g_prime = gee.prime(psibar);

  Mat ppmat  =stretch0(p_prime  ,nt+1);
  Mat ggpmat =stretch0(g*g_prime,nt+1);
  return (- r*r * ppmat - ggpmat);
}

Mat ceo::jacobian2D(void){

  /* poloidal Jacobian */

   //Vec t(nt+1);
   //t.space(0.0, TwoPi);
   Mat dz_dt   = dylag(t     , z);
   Mat dr_dt   = dylag(t     , r);
   Mat dz_dp   = dxlag(psibar, z);
   Mat dr_dp   = dxlag(psibar, r);
   Mat res = dr_dp;
   res *= dz_dt;
   res -= dr_dt*dz_dp;
   res += 1.0e-6;         // fixes singularity at origin
   return res;
}

Mat ceo::jacobian(void){

  /* 3-D Jacobian */

   Mat res = jacobian2D();
   res *= r;
   return res;
}

Mat ceo::diffArcLength(void){
  
  /* dl /d theta */

  Mat dz_dt2   = dylag(t     , z);
  dz_dt2 *= dz_dt2;
  Mat res   = dylag(t     , r);
  res *= res;
  res += dz_dt2;
  return sqrt(res);
}


Mat ceo::d_dr(Mat &f){

  /* partial derivative with respect to R = major radius */

    //Vec t(nt+1);
    //t.space(0.0, TwoPi);

  Mat dz_dt   = dylag(t     , z);
  Mat dz_dp   = dxlag(psibar, z);
  Mat df_dp   = dxlag(psibar, f);
  Mat df_dt   = dylag(     t, f);

  Mat res = dz_dt;
  res    *= df_dp;
  res -= dz_dp*df_dt;
  res /= jacobian2D();
  //reg_near_axis( res , psibar, 8 );
  return res;
}



Mat ceo::d_dz(Mat &f){

  /* partial derivative with respect to Z */

    //Vec t(nt+1);
    //t.space(0.0, TwoPi);

  Mat dr_dt   = dylag(t     , r);
  Mat dr_dp   = dxlag(psibar, r);
  Mat df_dp   = dxlag(psibar, f);
  Mat df_dt   = dylag(t     , f);

  Mat res = -dr_dt;
  res    *=  df_dp;
  res    +=  dr_dp*df_dt;
  res    /=  jacobian2D();
  //reg_near_axis( res , psibar, 8 );
  return res;
}

Mat ceo::Laplacian_star(void){
  /*
     apply the Grad-Shafranov operator on the poloidal flux psibar, 
     and return the result at every node. 
     */

  Mat psimat = stretch0(psibar, nt+1);
  Mat dp_dr_over_r = d_dr(psimat) / r;
  // **** reg_near_axis(dp_dr_over_r, psibar ); // temporary disabled!!

  Mat dp_dz = d_dz(psimat);
  // **** reg_near_axis(dp_dz, psibar );
  Mat res     = d_dr(dp_dr_over_r);
  res        *= r;
  res        += d_dz(dp_dz );
  //reg_near_axis(res,psibar, 4);

  return res;
}

Mat ceo::gradPsibarSq(void){
  /*
    | grad psibar |^2 
  */
  Mat psimat = stretch0(psibar, nt+1);
  Mat res = d_dr(psimat);
  res *= res;
  res += (d_dz(psimat) * d_dz(psimat));
  return res;
}

Mat ceo::gradThetaSq(void){
  /*
    | grad theta |^2 
  */
  //Vec t(nt+1);
  //t.space(0.0, TwoPi);
  Mat themat = stretch1(t, ns);
  Mat res = d_dr(themat);
  res *= res;
  res += (d_dz(themat) * d_dz(themat));
  return res;
}


Mat ceo::gradPsibarGradTheta(void){
  /*
    grad theta . grad psibar
  */
  //Vec t(nt+1);
  //t.space(0.0, TwoPi);
  Mat themat = stretch1(t     , ns);
  Mat psimat = stretch0(psibar, nt+1);
  Mat res = d_dr(themat);
  res *= d_dr(psimat);
  res += (d_dz(themat) * d_dz(psimat));
  return res;
}

Mat ceo::nDotGradPsibar(void){
  /* d psibar/d n */
  Mat psimat = stretch0(psibar, nt+1);
  Mat res = d_dr(psimat);
  res *=  dylag(t     , z);
  res -= d_dz(psimat) * dylag(t     , r);
  res /= ceo::diffArcLength();
  return res;
}


Mat ceo::BSq(void){
  /*
    B^2 
    */
  Mat res = gradPsibarSq();

  Mat r2  = r;
  r2     *= r;

  res /= r2;
  Mat gmat2 = stretch0( g*g , nt+1);
  res += gmat2/r2;
  return res;
}

Vec ceo::r_jParallel(void){
  /*
    <J.B>/<g/R^2> = - (<B^2> g'/g + p')/<1/R^2>
    */
  cubspline pres(psibar, p);
  cubspline gees(psibar, g);
  Mat one_over_r2(nt+1, ns, 1.0);
  one_over_r2 /= r;
  one_over_r2 /= r;

  Vec res = poloidalAverage( BSq() );
  res *= gees.prime(psibar);
  res /= g;
  res += pres.prime(psibar);
  res /= poloidalAverage( one_over_r2 );
  return (-res);
}
  
  
Vec ceo::poloidalIntegral(const Mat &in){
/*
  integral d theta jacobian ()
  */
  Mat jac = jacobian();
  return (TwoPi*average0( jac*in ));
}

Vec ceo::poloidalAverage(const Mat &in){
/*
  integral d theta jacobian () / integral d theta jacobian 
  */
  Mat jac = jacobian();
  return (average0( jac*in )/average0( jac ));
}

void ceo::radialRemap(int is=2){
  /* 
     interpolate onto new radial uniform grid in psi^(is/2)
     */

    unsigned int nt1 = ceo::nt + 1;
    Real ihalf = Real(is)/2.0;
    Real inv_ihalf = 1.0/ihalf;

    Vec sold = ceo::s;
    Real psimax = max(ceo::psi);
    for (unsigned int i=0; i<ceo::ns; i++) {
      ceo::s(i) = pow(ceo::psi(i)/psimax, ihalf);
      ceo::psi(i) = psimax* pow(ceo::s(i), inv_ihalf);
      ceo::psibar(i) = ceo::psi(i)/TwoPi;
    }

    cubspline phi_spl(sold, ceo::phi);
    ceo::phi = phi_spl.cubic(ceo::s);

    ceo::phibar = ceo::phi/TwoPi;

    cubspline g_spl(sold, g);
    g = g_spl.cubic(ceo::s);
    
    cubspline iota_spl(sold, iota);
    iota = iota_spl.cubic(ceo::s);
    
    cubspline p_spl(sold, p);
    p = p_spl.cubic(ceo::s);

    Mat rold = ceo::r;
    Mat zold = ceo::z;
    Vec told = t;
    jsointerp2d_(&nt1   ,&ns   ,&told(0)   ,&sold(0)   ,&rold(0,0), 
		 &nt1   ,&ns   ,&ceo::t(0)   ,&ceo::s(0), &ceo::r(0,0));
    jsointerp2d_(&nt1   ,&ns   ,&told(0)   ,&sold(0)   ,&zold(0,0), 
		 &nt1   ,&ns   ,&ceo::t(0)   ,&ceo::s(0), &ceo::z(0,0));

}

void ceo::poloidalRemap(int m=1, int n=1, int l=0){
  /* 
     remap onto new grid with Jacobian specified by 
     dependence ~  R^m/(|grad psi|^n B^l)
     */

    unsigned int nt1 = nt + 1;
    Mat integrand = jacobian();
    Real enhalf = Real(n)/2.0;
    Real elhalf = Real(l)/2.0;
    integrand *= pow( gradPsibarSq(), enhalf);
    integrand *= pow( BSq(), elhalf);
    integrand /= pow(r, (double)m);
    Vec_int I(nt1);
    I.range(0);

    char per[9] = "periodic";
    for(unsigned int j=0; j<ns; ++j) {

      // get new theta vs old theta

      cubspline intspl(t,integrand(I,j), per);
      Vec newT = intspl.integrate(t);
      newT /= newT(nt);
      newT *= TwoPi;

      // interpolate r and z onto newT, such that
      // newT increases by a constant amount (= increment in t)

      cubspline rspl(newT, r(I,j), per);

      Vec newr1 = rspl.cubic(t);
      cubspline zspl(newT, z(I,j), per);
      Vec newz1 = zspl.cubic(t);

      for(unsigned int i=0; i<nt1; i++){
	r(i,j) = newr1(i);
	z(i,j) = newz1(i);
      }
    }
}



Real ceo::GSRelError(void){
  
  // Rel. error of Grad Shafranov equation

  Mat rjphi = r_jphi();
  Mat integrand = Laplacian_star();
  integrand    -= rjphi;
  integrand    *= jacobian();
  integrand    /= max(rjphi);
  
  //Real res = sum(sum0(integrand))/(Real(nt)*Real(ns));

  Real res = 0.0;
  for(unsigned int j=1; j<integrand.size(1); ++j){
    for(unsigned int i=0; i<integrand.size(0); ++i) {
      res += integrand(i,j)*(psibar(j)-psibar(j-1));
    }
  }
  res /= Real(nt)*(max(psibar)-min(psibar));

  return res;
}

void ceo::toOrbit(std::string filename){
  

  // Orbit code  file format

  /* save object in netcdf file format */
  
  EZcdf file(filename, (std::string)"w");
  
  // Define variables

  std::string xtype_c = "CHAR";
  std::string xtype_i = "INT";
  std::string xtype_d = "DOUBLE";
  int dimlens_1[] = {1, 1, 1};
  int dimlens_ns[] = {1, 1, ns};
  int dimlens_nt[] = {1, 1, nt+1};
  int dimlens_nt_ns[] = {1, ns, nt+1};

  const int SIZE = 26;
  char buffer[SIZE];
  time_t curtime;
  struct tm *loctime;

  /* Get the current time.  */
  curtime = time (NULL);

  /* Convert it to local time representation.  */
  loctime = localtime (&curtime);

  /* Print it out in a nice format.  */
  strftime (buffer, SIZE, "%a %d %b %Y %X", loctime);
  cout << buffer << endl;
   
  int dimlens_c[] = {1, 1, SIZE};
  file.cdfDefVar((std::string)"today", dimlens_c, xtype_c);
  dimlens_c[2] = (int)(label.length()+1);
  file.cdfDefVar((std::string)"label", dimlens_c, xtype_c);
  file.cdfDefVar((std::string)"mth", dimlens_1, xtype_i);
  file.cdfDefVar((std::string)"nosurf", dimlens_1, xtype_i);
  file.cdfDefVar((std::string)"xma", dimlens_1, xtype_d);
  file.cdfDefVar((std::string)"zma", dimlens_1, xtype_d);
  file.cdfDefVar((std::string)"s", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"t", dimlens_nt, xtype_d);
  file.cdfDefVar((std::string)"psibig", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"p", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"g", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"q", dimlens_ns, xtype_d);
  file.cdfDefVar((std::string)"r", dimlens_nt_ns, xtype_d);
  file.cdfDefVar((std::string)"z", dimlens_nt_ns, xtype_d);
  file.cdfDefVar((std::string)"xjacob", dimlens_nt_ns, xtype_d);
  file.cdfDefVar((std::string)"grpssq", dimlens_nt_ns, xtype_d);

  char *label_c = (char *)label.c_str();
  int snt = (int)nt;
  int sns = (int)ns;
  cout << " label " << label << endl;
  file.cdfPutVar((std::string)"today", buffer);
  file.cdfPutVar((std::string)"label", label_c);
  file.cdfPutVar((std::string)"mth", &snt);
  file.cdfPutVar((std::string)"nosurf", &sns);
  file.cdfPutVar((std::string)"xma", &rmag);
  file.cdfPutVar((std::string)"zma", &zmag);
  file.cdfPutVar((std::string)"s", &s(0));
  file.cdfPutVar((std::string)"t", &t(0));
  file.cdfPutVar((std::string)"psibig", &psi(0));
  file.cdfPutVar((std::string)"p", &p(0));
  file.cdfPutVar((std::string)"g", &g(0));
  Vec q = 1.0/iota;
  file.cdfPutVar((std::string)"q", &q(0));
  file.cdfPutVar((std::string)"r", &r(0,0));
  file.cdfPutVar((std::string)"z", &z(0,0));
  file.cdfPutVar((std::string)"xjacob", &jacobian()(0,0));
  file.cdfPutVar((std::string)"grpssq", &gradPsibarSq()(0,0));
}  


void plot(ceo &eq1, ceo &eq2){

  // compare profiles

  plotmtv P;
  P.display = false;
  P.xlabel = "~sqrt(phi)";
  Vec s1 = sqrt(eq1.phi)/max(sqrt(eq1.phi));
  Vec s2 = sqrt(eq2.phi)/max(sqrt(eq2.phi));

  P.toplabel = "p";
  P.linelabel = eq1.title;
  P.plot(s1, eq1.p,"b|","bx");
  P.hold = true;
  P.linelabel = eq2.title;
  P.plot(s2, eq2.p,"rl","rs");

  P.hold = false;
  P.toplabel = "q";
  P.linelabel = eq1.title;
  P.plot(s1, eq1.iota,"b|","bx");
  P.hold = true;
  P.linelabel = eq2.title;
  P.plot(s2, eq2.iota,"rl","rs");  

  P.hold = false;
  P.toplabel = "psi";
  P.linelabel = eq1.title;
  P.plot(s1, eq1.psi,"b|","bx");
  P.hold = true;
  P.linelabel = eq2.title;
  P.plot(s2, eq2.psi,"rl","rs");
  
  P.hold = false;
  P.toplabel = "g";
  P.linelabel = eq1.title;
  P.plot(s1, eq1.g,"b|","bx");
  P.hold = true;
  P.linelabel = eq2.title;
  P.plot(s2, eq2.g,"rl","rs");

  P.hold = false;
  P.toplabel = "grid";
  P.linelabel = eq1.title;
  P.xlabel  = "R";
  P.ylabel  = "Z";
  
      Vec rflat1 = flatten0(eq1.r);
      Vec zflat1 = flatten0(eq1.z);
      P.xlabel  = "R";
      P.ylabel  = "Z";
      P.plot(rflat1,zflat1,"b|");
      P.hold    = true;
      P.plot(eq1.ra, eq1.za,"b ","bs");

  P.linelabel = eq2.title;

      Vec rflat2 = flatten0(eq2.r);
      Vec zflat2 = flatten0(eq2.z);
      P.plot(rflat2,zflat2,"rl");
      P.display = true;
      P.plot(eq2.ra, eq2.za,"r ","ro");
}

#endif // __ceo__
