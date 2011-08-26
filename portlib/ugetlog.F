      subroutine ugetlog(zbuff)
      character*(*) zbuff               ! for username string, output
c
c  return the current username
c
#ifdef __UNIX
c
      character*80 homedir
      character*150 ztmpfile
      integer ilz,iln,str_length,ier
c
      call sget_env('LOGNAME',zbuff)
      if(zbuff.eq.' ') then
         call sget_env('USER',zbuff)
         if(zbuff.eq.' ') then
c
c  try a "whoami" into a temporary file
c
            call sget_env('HOME',homedir)
            if(homedir.eq.' ') then
               zbuff='UNKNOWN'
            else
               iln=str_length(homedir)
               if(homedir(iln:iln).eq.'/') iln=iln-1
               call tmpfile(homedir(1:iln)//'/ugetlog_',ztmpfile,ilz)
               call jsystem('whoami > '//ztmpfile(1:ilz))
               call read_word(ztmpfile(1:ilz),zbuff,ier)
               if(ier.ne.0) zbuff=' '
               if(zbuff.eq.' ') then
                  zbuff='UNKNOWN'
               endif
               call fdelete(ztmpfile(1:ilz),ier)
            endif
         endif
      endif
#endif
      return
      end
