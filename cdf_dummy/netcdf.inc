! Dummy include for dummy netcdf
!
      integer nf_max_name
      parameter (nf_max_name = 128)
      integer nf_global
      parameter (nf_global = 0)

      integer nf_noerr
      parameter (nf_noerr = 0)

      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)


      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)


      integer nf_inq_varid
      integer nf_inq_var
      integer nf_inq_dim
      integer nf_inq_dimlen
      integer nf_inq_dimid
      integer nf_def_dim
      integer nf_def_var
      integer nf_put_att_text
      integer nf_put_att_int
      integer nf_put_att_real
      integer nf_put_att_double
      integer nf_enddef
      integer nf_put_var_real
      integer nf_put_var_text
      integer nf_put_var_int
      integer nf_put_var_double
      integer nf_put_vara_double
      integer nf_put_vara_real
      integer nf_put_vara_text
      integer nf_put_vara_int
      integer nf_get_att_text
      integer nf_get_att_int
      integer nf_get_att_real
      integer nf_get_att_double
      integer nf_get_var_double
      integer nf_get_var_real
      integer nf_get_vara_text
      integer nf_get_var_text
      integer nf_get_var_int


      external nf_inq_varid
      external nf_inq_var
      external nf_inq_dim
      external nf_inq_dimlen
      external nf_inq_dimid
      external nf_def_dim
      external nf_def_var
      external nf_put_att_text
      external nf_put_att_int
      external nf_put_att_real
      external nf_put_att_double
      external nf_enddef
      external nf_put_var_real
      external nf_put_var_text
      external nf_put_var_int
      external nf_put_var_double
      external nf_put_vara_double
      external nf_put_vara_real
      external nf_put_vara_text
      external nf_put_vara_int
      external nf_get_att_text
      external nf_get_att_int
      external nf_get_att_real
      external nf_get_att_double
      external nf_get_var_double
      external nf_get_var_real
      external nf_get_vara_text
      external nf_get_var_text
      external nf_get_var_int
