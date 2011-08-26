/* zputc.c */
/* putchar callable from fortran on UNIX */
 
/* function names link differently depending on OS */
 
#include "fpreproc/f77name.h"
 
#ifdef __UNIX
 
#include <stdio.h>
 
#ifdef __SX
#define SYS5
#endif
 
#ifdef __SGI
#define SYS5
#endif
 
#ifdef __SUN
#undef SYS5
#endif
 
#ifdef __ALPHA
#define SYS5
#endif
 
#ifdef __IBM
#undef SYS5
#endif
 
#ifdef __HP
#undef SYS5
#endif
 
#ifdef __LINUX
#undef SYS5
#endif
 
#ifdef SYS5
#include <sys/termio.h>
#else		/* POSIX */
#include <termios.h>
#endif
 
#ifdef SYS5
struct termio init_tty, new_tty;
#else
struct termios init_tty, new_tty;
#endif
 
#endif  /* __UNIX */
 
int F77NAME(zgetc)()
{
#ifdef __UNIX
 
  int j,istat;
 
#ifdef SYS5
  ioctl(0, TCGETA, &init_tty);
  ioctl(0, TCGETA, &new_tty);
#else
  tcgetattr(0, &init_tty);
  tcgetattr(0, &new_tty);
/*  ioctl(0, TCGETP, &init_tty); */
/*  ioctl(0, TCGETP, &new_tty); */
#endif
  new_tty.c_lflag = new_tty.c_lflag & ~(ECHO); /* turn off echo */
  new_tty.c_lflag = new_tty.c_lflag & ~(ICANON); /* turn off canonical */
  new_tty.c_cc[VMIN] = 1;		/* one character input */
  new_tty.c_cc[VTIME] = 0;	/* timer off */
#ifdef SYS5
  ioctl(0, TCSETA, &new_tty);
#else
  tcsetattr(0, TCSANOW, &new_tty);
/*  ioctl(0, TCSANOW, &new_tty); */
#endif
 
  istat = fflush(NULL);
  j = (int) getchar();
 
/* canon(); reset to canonical mode */
#ifdef SYS5
  ioctl(0, TCSETA, &init_tty);
#else
  tcsetattr(0, TCSANOW, &init_tty);
/*  ioctl(0, TCSANOW, &init_tty); */
#endif
 
  return j;
 
#else
  return 0;
#endif   /* UNIX or not */
 
}
