/* c24dat0.c:
 
   fdate subroutine for fortran.
   ANSI C replacement for fdate.
   Mimic SGI fdate.
 
   (c) D. R. Ernst <dernst@pppl.gov>  7/29/95, 5/10/99
   Permission granted only for royalty-free not-for profit distribution.
 
   byte udate(24)
   call c24dat0(udate)
 
   Note format can be changed by specifying format constant.
   See man page for strftime.
 
*/
 
 
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include "fpreproc/f77name.h"
 
void F77NAME(c24dat0)(zdate,zlen)
char zdate[];
int *zlen;
{
#define LEN 24
 
    int i;
    time_t TimeNow;
    struct tm *TimeNowStruct;
    size_t out_len = LEN;
    char out[LEN];
    size_t iret;
    char *format="%d-%b-%Y %H:%M:%S";
    /* equiv to command  date +'%d-%b-%Y %H:%M:%S' */
 
#ifdef __DEBUG_DATE
    printf("%s\n", "entry" );
    printf("Format: %s\n", format);
#endif
 
    time ( &TimeNow );
 
#ifdef __DEBUG_DATE
    printf("Time is now %s\n", ctime( &TimeNow));
    printf("%s\n", "Calling localtime...");
#endif
 
    TimeNowStruct = localtime ( &TimeNow );
 
#ifdef __DEBUG_DATE
    printf("%s\n", "return from localtime...");
    printf( "asctime gives %s" , asctime ( localtime ( &TimeNow ) ) ) ;
    printf( "TimeNowStruct gives %s" , asctime ( TimeNowStruct) );
#endif
 
    iret = strftime( out, out_len, format, TimeNowStruct );
 
#ifdef __DEBUG_DATE
    printf( "Converted: %s\n", out );
#endif
 
    for (i=0; i < *zlen; i++){
      zdate[i] =  ' ';
    }
    for (i=0; i < out_len && i < *zlen; i++ ){
      zdate[i] =  out[i];
    }
}
 
 
 
 
 
 
