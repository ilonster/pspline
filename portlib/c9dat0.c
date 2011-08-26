/* c9dat0.c:
 
   ANSI C date routine sort of like SGI fdate.
   new output format:  ddmmmyyyy e.g. 05mar1993
 
   (c) D. R. Ernst <dernst@pppl.gov>  7/29/95, 5/10/99
   Permission granted only for royalty-free not-for profit distribution.
 
   byte udate(9)
   call c9dat0(udate)
 
   Note format can be changed by specifying format constant.
   See man page for strftime.
 
*/
 
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include "fpreproc/f77name.h"
 
void F77NAME(c9dat0)(zdate,zlen)
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
    char *format="%d%b%Y";
    /* equiv to command  date +'%d%b%Y' */
 
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
    printf( "Converted: length: %d str: %s\n", out_len, out );
#endif
 
    for (i=0; i < *zlen; i++){
      zdate[i] =  ' ';
    }
    for (i=0; i < out_len && i < *zlen; i++ ){
      zdate[i] =  out[i];
    }
}
 
 
 
 
 
 
 
