#ifdef __CRAY__ 
#define jsoinit_ JSOINIT
#define jsoexec_ JSOEXEC
#define jsofree_ JSOFREE
#define jsosetipsi_ JSOSETIPSI
#define jsosetieqdsk_ JSOSERIEQDSK
#define jsogetp_ JSOGETP
#define jsosetiverbose_ JSOIVERBOSE
#define jsogetp_ JSOGETP
#define jsogetq_ JSOGETQ
#define jsogetg_ JSOGETG
#define jsogetgp_ JSOGETGP
#define jsogetpsi_ JSOGETPSI
#define jsointerp2d_ JSOINTERP2D
#endif

#if defined( __AIX__) || defined(__HPX__) || defined(__IBM__) 
#define jsoinit_ jsoinit
#define jsoexec_jsoexec
#define jsofree_ jsofree
#define jsosetipsi_ jsosetipsi
#define jsosetieqdsk_ jsosetieqdsk
#define jsogetp_ jsogetp
#define jsosetiverbose_ jsosetiverbose
#define jsogetp_ jsogetp
#define jsogetq_ jsogetq
#define jsogetg_ jsogetg
#define jsogetgp_ jsogetgp
#define jsogetpsi_ jsogetpsi
#define jsointerp2d_ jsointerp2d
#endif

