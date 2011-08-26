/* define byte_declare as a macro for declaration of an f77 byte array */

#ifndef BYTE_DECLARE

#if __SUN || __SUNOS 
#if __F90
#define BYTE_DECLARE integer*1
#else
#define BYTE_DECLARE byte
#endif
#elif __HP
#define BYTE_DECLARE byte
#elif __CRAY || __SX
#define BYTE_DECLARE character*1
#else
#define BYTE_DECLARE integer*1
#endif

#endif
