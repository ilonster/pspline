#ifndef _CONST__ /* define (floating point) CONST macro just once */
#define _CONST__

#ifndef __DOUBLE_PRECISION
#ifndef __SINGLE_PRECISION
#ifdef __CRAY
#define __SINGLE_PRECISION
#else
#define __DOUBLE_PRECISION
#endif
#endif
#endif

#ifdef __DOUBLE_PRECISION
#ifdef __SX
#define CONST(mant,exp)  mant D exp
#else
#define CONST(mant,exp)  mant##D##exp
#endif
#endif

#ifdef __SINGLE_PRECISION
#define CONST(mant,exp)  mant##E##exp
#endif

#endif
