C *****************************
C  DUMMY HDF Library Routines *
C *****************************
C
C These routines are linked in, unless
C LOADER_TRAILER is defined to point to netcdf libraries
C e.g.  export LOADER_TRAILER='-L/usr/local/lib -lnetcdf'
C
C 08/13/97 CAL
C
C  4/8/99 dmc -- GTRNLOG call removed (portability problem)
C                nf_open errror message shortened.
C
C
C   modification to return special error code on dummy open call
C
C Contents:
 
      integer function nf_open(filename, access, id)
      character*(*) filename
      integer       access, id
      integer  l, istat
      character*(10) tnam
 
      write(6,1001)
 1001 format(' %cdf_dummy:  NetCDF library not linked.')
 
      nf_open = -7777
      return
      end
 
      integer function nf_create(filename, access, id)
      character*(*) filename
      integer       access, id
 
      write(6,1001)
 1001     FORMAT(/
     >  '? cdf_dummy called: source/generic_dummy/cdf_dummy.for    '/
     >  '                                                          '/
     >  '      To link with ''real'' netCDF library                '/
     >  '      define LOADER_TRAILER in .transprc (or equiv)       '/
     >  '                                                          '/
     >  '      On Unix:                                            '/
     >  '      e.g.:                                               '/
     >  '      export LOADER_TRAILER=''-L/usr/local/lib -lnetcdf'' '/
     >  '                                                          '/
     >  '      On VMS:                                             '/
     >  '      re-define logical TRAILER_OPTFILE in login.com (etc)'/
     >  '      e.g.                                                '/
     >  '      assign MY_TRAILER.OPT  TRAILER_OPTFILE              '/
     >  '                                                          '/
     >  '                 ( CAL  08/13/97  )                       ')
 
      nf_create = -7777
      return
      end
C
      integer function nf_close(file_id)
      nf_close = -1
      return
      end
C
      integer function nf_inq(id, dim, nvars, ngatts, unlimid)

      nf_inq = -1
      return
      end
C
      integer function nf_inq_var(id,varid,name,xtype,ndims,dimids,nat)
      character(*)        name
      integer             dimids(1)

      nf_inq_var = -1
      return
      end
C
      integer function nf_inq_dim(id, did, dimnam, len)
      nf_inq_dim = -1
      return
      end
C
      integer function nf_inq_dimlen(id, did, len)
      nf_inq_dimlen = -1
      return
      end
C
      integer function nf_inq_dimid(id, did, len)
      nf_inq_dimid = -1
      return
      end
C
      integer function nf_inq_unlimdim(id, did)
      nf_inq_unlimdim = -1
      return
      end
C
      integer function nf_inq_varname(id, varid, nam)
      nf_inq_varname = -1
      return
      end
C
      integer function nf_inq_vardimid(id, varid, did)
      nf_inq_vardimid = -1
      return
      end
C
      integer function nf_inq_varid(id, nam, varid)
      character(*)        nam
      nf_inq_varid = -1
      return
      end
C
      integer function nf_inq_attlen(id, varid, nam, l)
      character(*)        nam
      nf_inq_attlen = -1
      return
      end
C
      integer function nf_def_var(id,nam,xtyp,ndim,vdim,vals)
      character(*)        nam
      nf_def_var = -1
      return
      end
C
      integer function  nf_def_dim(id,nam,ndim,vdim)
      character(*)        nam
      nf_def_dim = -1
      return
      end
C
      integer function nf_put_att_int(id,varid,nam,xtyp,l,ivals)
      character(*)        nam
      nf_put_att_int = -1
      return
      end
C
      integer function nf_put_att_real(id,varid,nam,xtyp,l,xvals)
      character(*)        nam
      nf_put_att_real = -1
      return
      end
C
      integer function nf_put_att_double(id,varid,nam,xtyp,l,xvals)
      character(*)        nam
      nf_put_att_double = -1
      return
      end
C
      integer function nf_put_att_text(id,varid,nam,l,text)
      character(*)        nam
      nf_put_att_text = -1
      return
      end
C
      integer function nf_put_att_int2(id,varid,nam,t,l,ival)
      character(*)        nam
      nf_put_att_int2 = -1
      return
      end
C
      integer function nf_put_var_real(id, varid, val)
      nf_put_var_real = -1
      return
      end
C
      integer function nf_put_var_int(id,varid,val)
      nf_put_var_int = -1
      return
      end
C
      integer function nf_put_var_int1(id, varid, val)
      nf_put_var_int1 = -1
      return
      end
C
      integer function nf_put_var_double(id,varid,val)
      nf_put_var_double = -1
      return
      end
C
      integer function nf_put_var_text(id,varid,val)
      nf_put_var_text = -1
      return
      end
C
      integer function nf_put_vara_int(id, varid, st,cnt,val)
      nf_put_vara_int = -1
      return
      end
C
      integer function nf_put_vara_double(id, varid, st,cnt,val)
      nf_put_vara_double = -1
      return
      end
C
      integer function nf_put_vara_text(id, varid, st, cnt, text)
      nf_put_vara_text = -1
      return
      end
C
      integer function nf_put_vara_real(id, varid, st,cnt,val)
      nf_put_vara_real = -1
      return
      end
C
      integer function nf_get_att_real(id,varid,nam,xvals)
      character(*)        nam
      nf_ get_att_real = -1
      return
      end
C
      integer function nf_get_att_double(id,varid,nam,xvals)
      character(*)        nam
      nf_ get_att_double = -1
      return
      end
C
      integer function nf_get_att_int(id,varid,nam,ivals)
      character(*)        nam
      nf_ get_att_int = -1
      return
      end
C
      integer function nf_get_att_int2(id,varid,nam,ivals)
      character(*)        nam
      nf_ get_att_int2 = -1
      return
      end
C
      integer function nf_get_att_text(id,varid,nam,text)
      character(*)        nam
      nf_get_att_text = -1
      return
      end
c
      integer function nf_get_var_real(id, vid, vals)
      nf_get_var_real = -1
      return
      end
C
      integer function nf_get_var_int1(id, vid, vals)
      nf_get_var_int1 = -1
      return
      end
C
      integer function nf_get_vara_text(id, vid, st, cnt, text)
      nf_get_vara_text = -1
      return
      end
C
      integer function nf_get_vara_real(id, vid, st, cnt, vals)
      nf_get_vara_real = -1
      return
      end
C
      integer function  nf_enddef(id)
      nf_enddef = -1
      return
      end
C
      integer function  nf_redef(id)
      nf_redef = -1
      return
      end
C
      integer function  nf_strerror(ierr)
      nf_strerror = -1
      return
      end
C
      integer function  nf_noerr(ierr)
      nf_noerr = -1
      return
      end
C
      integer function nf_inq_att(id,vid,name,itype,ilen)
      character(*)        name
      nf_inq_att = -1
      return
      end
C
      integer function nf_inq_varnatts(id,vid,inatt)
      nf_inq_varnatts = -1
      return
      end
C
      integer function nf_inq_attname(id,vid,aid,name)
      character(*)        name
      nf_inq_attname = -1
      return
      end
C
      integer function nf_rename_att(id,vid,oldname,newname)
      character(*)        oldname, newname
      nf_rename_att = -1
      return
      end
C
      integer function nf_rename_var(id,vid,newname)
      character(*)        newname
      nf_rename_var = -1
      return
      end
C
      integer function nf_inq_varndims(id,vid,ndims)
      nf_inq_varndims = -1
      return
      end
C
      integer function nf_get_var_double(id,vid,vals)
      nf_get_var_double = -1
      return
      end
C
      integer function nf_get_var_int(id,vid,ivals)
      nf_get_var_int = -1
      return
      end
C
      integer function nf_get_var_text(id,vid,cvals)
      nf_get_var_text = -1
      return
      end
C
      integer function nf_get_vara_double(id,vid,vals)
      nf_get_vara_double = -1
      return
      end
C
      integer function nf_get_vara_int(id,vid,ivals)
      nf_get_vara_int = -1
      return
      end
