#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "export.h" /*IDL export library for IDL typedefs, etc. */

#if UNDERSCORE
  #define FORT(name)  name ## _
#else
  #define FORT(name) name
#endif

#if UNDERSCORE
  #define FSUFFIX _
#else
  #define FSUFFIX
#endif

#define CSUFFIX _idl
#define CAT(a,b) a ## b
#define MCAT(a,b) CAT(a,b)
#define STR(a) #a


/*** TEST Routines ***/
float ftest(int argc, void *argv[])
{
  float x, ans;
  extern float TEST(float*);

/*  printf("ftest\n");*/
  x = *(float *) argv[0];

  ans = TEST(&x);
  ans = 2.0*TEST((float*) argv[0]);
/*  printf("%e\n",ans);*/

  return ans;
}


#define PRO  test2d
#define NARG 4
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(int*, int*, float**,float**);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((int*)argv[0],(int*)argv[1],(float**)argv[2],(float**)argv[3]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO



/*******************************
    EXPLICIT SPLINE ROUTINES
*******************************/

/*
c  cspline -- dmc 15 Feb 1999
c
c  a standard interface to various 1d spline setup routines
c
      subroutine cspline(x,nx,fspl,ibcxmin,bcxmin,ibcxmax,bcxmax,
     >   wk,iwk,ilinx,ier)
c
      real x(nx)                  ! x axis (in)
      real fspl(4,nx)             ! spline data (in/out)
      integer ibcxmin             ! x(1) BC flag (in, see comments)
      real bcxmin                 ! x(1) BC data (in, see comments)
      integer ibcxmax             ! x(nx) BC flag (in, see comments)
      real bcxmax                 ! x(nx) BC data (in, see comments)
      real wk(iwk)                ! workspace of size at least nx
c
c  workspace only used if "periodic" boundary condition is selected.
c
      integer ilinx               ! even spacing flag (out)
      integer ier                 ! output status, =0 means OK
*/
#define PRO  cspline
#define NARG 11
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,int*,float**,int*,float*,int*,float*,float*,int*,int*,int*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(int*)argv[1],(float**)argv[2],(int*)argv[3],(float*)argv[4],(int*)argv[5],(float*)argv[6],(float*)argv[7],(int*)argv[8],(int*)argv[9],(int*)argv[10]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO

/*
      subroutine bcspline(x,inx,th,inth,fspl,inf3,
     >                    ibcxmin,bcxmin,ibcxmax,bcxmax,
     >                    ibcthmin,bcthmin,ibcthmax,bcthmax,
     >                    wk,nwk,ilinx,ilinth,ier)
c
      real x(inx),th(inth),fspl(4,4,inf3,inth),wk(nwk)
      real bcxmin(inth),bcxmax(inth)
      real bcthmin(inx),bcthmax(inx)
*/ 
#define PRO  bcspline
#define NARG 19
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,int*,float*,int*,float****,int*,    \
                     int*,float*,int*,float*,                   \
                     int*,float*,int*,float*,                   \
                     float*,int*,int*,int*,int*);   

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(int*)argv[1],(float*)argv[2],(int*)argv[3],(float****)argv[4],(int*)argv[5],
        (int*)argv[6],(float*)argv[7],(int*)argv[8],(float*)argv[9],
        (int*)argv[10],(float*)argv[11],(int*)argv[12],(float*)argv[13],
        (float*)argv[14],(int*)argv[15],(int*)argv[16],(int*)argv[17],(int*)argv[18]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
c  tcspline -- dmc 20 Jan 1999
c
c  set up coefficients for bicubic spline with following BC's:
c  * LHS and RHS handled as in cubspl.for for 1st coordinate
c  * derivatives periodic in second coordinate (use pspline.for)
c
      subroutine tcspline(x,inx,th,inth,ph,
     4                     inph,fspl,inf4,inf5,
     >                    ibcxmin,bcxmin,ibcxmax,bcxmax,inb1x,
     >                    ibcthmin,bcthmin,ibcthmax,bcthmax,inb1th,
     >                    ibcphmin,bcphmin,ibcphmax,bcphmax,inb1ph,
     >                    wk,nwk,ilinx,ilinth,ilinph,
     1                     ier)
c
      real x(inx),th(inth),ph(inph)
      real fspl(4,4,4,inf4,inf5,inph),wk(nwk)
      real bcxmin(inb1x,inph),bcxmax(inb1x,inph) ! inth x inph used
      real bcthmin(inb1th,inph),bcthmax(inb1th,inph) ! inx x inph used
      real bcphmin(inb1ph,inth),bcphmax(inb1ph,inth) ! inx x inth used
*/ 
#define PRO  tcspline
#define NARG 30
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,int*,float*,int*,float*, 	\
                     int*,float****,int*,int*,          \
                     int*,float*,int*,float*,int*,      \
                     int*,float*,int*,float*,int*,      \
                     int*,float*,int*,float*,int*, 	    \
                     float*,int*,int*,int*,int*,int*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(int*)argv[1],(float*)argv[2],(int*)argv[3],(float*)argv[4],   \
        (int*)argv[5],(float****)argv[6],(int*)argv[7],(int*)argv[8],                   \
        (int*)argv[9],(float*)argv[10],(int*)argv[11],(float*)argv[12],(int*)argv[13], \
        (int*)argv[14],(float*)argv[15],(int*)argv[16],(float*)argv[17],(int*)argv[18], \
        (int*)argv[19],(float*)argv[20],(int*)argv[21],(float*)argv[22],(int*)argv[23], \
        (float*)argv[24],(int*)argv[25],(int*)argv[26],(int*)argv[27],(int*)argv[28],(int*)argv[29]);  
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
c  cspeval -- eval cubic spline function and/or derivatives
c
      subroutine cspeval(xget,iselect,fval,x,nx,ilinx,f,ier)
c
      real xget                         ! interpolation target
      real fval(3)                      ! output values
      real x(nx),f(4,nx)                ! spline data
c
      integer iselect(3)                ! output selector
      integer ilinx                     ! =1 if x(...) is evenly spaced
*/
#define PRO  cspeval
#define NARG 8
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,int*,float*,float*,int*,int*,float**,int*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(int*)argv[1],(float*)argv[2],(float*)argv[3],(int*)argv[4],(int*)argv[5],(float**)argv[6],(int*)argv[7]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
c  bcspeval -- eval bicubic spline function and/or derivatives
c
      subroutine bcspeval(xget,yget,iselect,fval,
     >                    x,nx,y,ny,ilinx,iliny,f,inf3,ier)
c
      real xget,yget
      real fval(6)
      real x(nx),y(ny),f(4,4,inf3,ny)
c
      integer iselect(6)
      integer ilinx,iliny,nx,ny,inf3,ier
*/
#define PRO  bcspeval
#define NARG 13
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,float*,int*,float*,
                    float*,int*,float*,int*,int*,int*,
                    float****,int*,int*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(float*)argv[1],(int*)argv[2],(float*)argv[3],
        (float*)argv[4],(int*)argv[5],(float*)argv[6],(int*)argv[7],(int*)argv[8],(int*)argv[9],
        (float****)argv[10],(int*)argv[11],(int*)argv[12]);

    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
c  tcspeval -- eval tricubic spline function and/or derivatives
c
      subroutine tcspeval(xget,yget,zget,iselect,fval,
     >                    x,nx,y,ny,z,nz,ilinx,iliny,ilinz,f,inf4,inf5,
     >                    ier)
c
      real xget,yget,zget
      real fval(10)
      real x(nx),y(ny),z(nz),f(4,4,4,inf4,inf5,nz)
c
      integer iselect(10)
      integer ilinx,iliny,ilinz,nx,ny,nz,inf4,inf5,ier
c
*/
#define PRO  tcspeval
#define NARG 18
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(float*,float*,float*,int*,float*,
                    float*,int*,float*,int*,float*,int*,
                    int*,int*,int*,
                    float******,int*,int*,int*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((float*)argv[0],(float*)argv[1],(float*)argv[2],(int*)argv[3],(float*)argv[4],
         (float*)argv[5],(int*)argv[6],(float*)argv[7],(int*)argv[8],(float*)argv[9],(int*)argv[10],
         (int*)argv[11],(int*)argv[12],(int*)argv[13],
         (float******)argv[14],(int*)argv[15],(int*)argv[16],(int*)argv[17]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO

/*******************************
    OTHER 1D SPLINE ROUTINES
*******************************/

/*
      SUBROUTINE PSPLINE (N, X, Y, B, C, D, WK)
      INTEGER N
      REAL X(N), Y(N), B(N), C(N), D(N), WK(N)
*/
#define PRO  pspline
#define NARG 7
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(int*,float*,float*,float*,float*,float*,float*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((int*)argv[0],(float*)argv[1],(float*)argv[2],(float*)argv[3],(float*)argv[4],(float*)argv[5],(float*)argv[6]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO

/*
      SUBROUTINE SPLINE (N, X, Y, B, C, D)
      INTEGER N
      REAL X(N), Y(N), B(N), C(N), D(N)
*/
#define PRO  spline
#define NARG 6
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(int*,float*,float*,float*,float*,float*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((int*)argv[0],(float*)argv[1],(float*)argv[2],(float*)argv[3],(float*)argv[4],(float*)argv[5]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
      REAL FUNCTION SEVAL(N, U, X, Y, B, C, D)
      INTEGER N
      REAL  U, X(N), Y(N), B(N), C(N), D(N)
*/
#define PRO  seval
#define NARG 7
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
float CPRO(int argc, void *argv[])
{
    extern float FPRO(int*,float*,float*,float*,float*,float*,float*);
    float ans;

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    ans=FPRO((int*)argv[0],(float*)argv[1],(float*)argv[2],(float*)argv[3],(float*)argv[4],(float*)argv[5],(float*)argv[6]);
    return ans;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*
      REAL FUNCTION SPEVAL(N, U, X, Y, B, C, D)
      INTEGER N
      REAL  U, X(N), Y(N), B(N), C(N), D(N)
*/
#define PRO  speval
#define NARG 7
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
float CPRO(int argc, void *argv[])
{
    extern float FPRO(int*,float*,float*,float*,float*,float*,float*);
    float ans;

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    ans=FPRO((int*)argv[0],(float*)argv[1],(float*)argv[2],(float*)argv[3],(float*)argv[4],(float*)argv[5],(float*)argv[6]);
    return ans;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO


/*******************************
    V_SPLINE COEFFICIENT SOLVER
*******************************/

/*
       SUBROUTINE V_SPLINE(k_bc1,k_bcn,n,x,f,wk)
*/
#define PRO  v_spline
#define NARG 6
#define CPRO MCAT(PRO,CSUFFIX) 
#define FPRO MCAT(PRO,FSUFFIX)
int CPRO(int argc, void *argv[])
{
    extern void FPRO(int*,int*,int*,float*,float**,float*);

    if(argc != NARG) {
        printf("%s: requires %i arguments",STR(CPRO),NARG);
        return 1;
    }
    FPRO((int*)argv[0],(int*)argv[1],(int*)argv[2],(float*)argv[3],(float**)argv[4],(float*)argv[5]);
    return 0;
}
#undef FPRO
#undef CPRO
#undef NARG
#undef PRO

