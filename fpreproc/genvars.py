
import re
import string
import sys

#
# ======================= genvars.py =========================
# This file provides a set of classes and functions to aid
# in supporting the definition of a large number of fortran
# variables through automatic code generation.
#
# You will have one file containing variable definitions
# such as,
# 
#   @eta_par_r(mxnr_r)  
#    ftype:    Real(kind=rspec)          
#    ctype:    double
#    comment:  parallel electrical resistivity 
#    units:    (Ohm*m)
# 
# which is read in by
#
#   rvars = read_var_lines("filename")
#   var   = build_vars(rvars)
#
# this results in var, a list of vardef objects, which
# can generate code.
#
# Another file acts as a skeleton for inserting the code
#
#   fin  = open("myskeleton.skel",'r')
#   fout = open("mycode.f90",'w')
#   marker = "-----<<<<< GENERATED CODE MARKER >>>>>-----"
#
# where lines containing the marker line exist in myskeleton.skel
# at each point you want to insert generated code:
#
#   copy_to_marker(fin, fout, "myskeleton.skel", marker)  # copy fin to fout up to marker
#   for x in build_fortran_def(var):
#      fout.write(x+"\n")                      # add fortran definitions
#   ... and so on until ...
#   copy_to_marker(fin, fout, "myskeleton.skel")  # finish off the file
#   fout.close()
#
# and your generated code should be in "mycode.f90".
#
# ========================= format =================================
# @name                -> defines the start of a scalar
#
# @name(dim1,dim2,...) -> defines the start of an array
#     dim? -> names of the dimensions
#
# @default             -> start of a template to use as the default
#                         for all definitions which follow
#
# comment:  <comment line>              -> comment
# ftype:    <fortran type declaration>  -> e.g.  Real*8
# ctype:    <C type declaration>        -> e.g.  double
# units:    <units string>              -> e.g.  (keV), (-) for dimensionless
# init:     <where>  <value>            -> initialization value associated with section <where>
#                                          <where>=def -> put initialization on definition line (omit value to remove default)
# allocate: <value>                     -> <value> = "Yes","true",1 or "No","false",0 for allocation
#                                          of the variable or "pointer" for pointer type
# check:    <value> ...                 -> allocate: If this is an allocation variable, then check to make sure it is allocated
#                                                    before putting in the box or remove it if it is not allocated.
#                                          noallocate: do not check allocation status before putting in box (default)
#                                          size: make sure that all the dimensions are greater than 1 before putting in box
#                                          nosize: do not check dimension sizes (default)
# tag:      <name>   <value>            -> general storage of name,value pairs (omit value to remove default)
# fstruct   <name>                      -> this variable is part of a fortran type called <name>
#
# # -> comment line is ignored
#
# ========================== special tags ==========================
#
# tag: axis <type>      -> mark this variable as an axis.  The variable should be one dimensional
#                          and monotonically increasing.
#                          <type> -> types from java NumericAxisType (Index,Rmajor,X,Y,Z,...)
#                          If <type>=phony, then this is not a real variable, it represents a variable
#                          defined elsewhere which is an axis.
#
#
# =========================== examples ==============================
#@default
# ftype:   integer
# ctype:   int
# init:    allocate 0
#
##
## ------ Integer definitions -----
##
#@nitx(mx_mi)
# comment:  (D) mapping from isotope index to species index
#
#@idiag
# comment:  (D) species index of diagnostic impurity
#
#@ipsdup(mx_mi)
# comment:  0 if no isotopes with same psudo Z else duplicate index, (H,D,T distinct)
# init:     allocate 0
#
##
## --------------------------------------
## -------- make default a REAL ---------
## --------------------------------------
##
#@default
# ftype:   Real(kind=rspec)
# ctype:   float
# init:    allocate 0.0_rspec
# tag:     ncg_add_profile 1
# tag:     rplot_add 1
#
##
## ------ Real definitions -----
##
#@c_den  
# comment: (D) density cutoff below which species is ignored 
# units:   (1/m**3)
# init:    allocate 1.0e10_rspec
# tag:     rplot_add 0
#
#@amu(mx_mi)
# comment:  (D) atomic mass number of isotope
# units:    (-)
#
#@temp(mxnr_r, mx_mi)
# comment:  (D) temperature profile of isotope
# units:    (keV)
# tag:      nprof_name T
# tag:      rplot_name temp,'temp_'//trim(isotope_name(j))
# tag:      rplot_label 'temperature of '//trim(isotope_name(j))
# ====================================================================
#
# --------------------- truefalse -----------------------
#
def truefalse(input):
    """
    return 1 if true or 0 if false based on the input string
    e.g.
      input = '1', 'yes', 'True' -> gives true  (1)
      input = '0', 'nO', 'false' -> gives false (0)
    """
    a = string.lower(string.strip(input))
    if (a=="1" or a=="yes" or a=="true" or a=="yep"):
        return 1
    elif (a=="0" or a=="no" or a=="false" or a=="nope"):
        return 0
    raise RuntimeError("?genvars::truefalse(): Could not decide truth of string " + input)

#
# --------------------- clean_units -----------------------
#
def clean_units(s):
    """
    return the unit string 's' without the enclosing ()
    e.g.
       '(m/s)' -> 'm/s'
    """
    q = string.strip(s)
    n = len(q)
    ib=0         # starting index without (
    ie=n         # ending index without )
    if (n>0):
        if (q[0]=='('):  ib=1
        if (q[-1]==')'): ie=n-1
        return q[ib:ie]
    else:
        return q

#
# ------------------ no_tick ----------------
#
def no_tick(s):
    if ("'" in s):
        raise RuntimeError("the following string contains a ' character\n"+s)

#
# -------------------------- class vardef -------------------------------
#
rname   = re.compile(r".*@\s*([^\(\s]+)[^\(]*(.*)")  # name(dim1,dim2,..)  -> (name,rest)
rattr   = re.compile(r"\s*([^\s]+):(.*)")            #   something: ...    -> (something, rest)
respace = re.compile(r"\s")
re_int_type = re.compile(r"integer", re.IGNORECASE)
re_r8_type  = re.compile(r"real\s*\*\s*8", re.IGNORECASE)
re_double_type  = re.compile(r"double", re.IGNORECASE)
re_real_type  = re.compile(r"real", re.IGNORECASE)
re_logical_type  = re.compile(r"logical", re.IGNORECASE)

class vardef:
    """
    defines the name and attributes of a single variable
    """
    
    more_info = 0    # set true to get more info during print of these objects
    default   = 0    # a vardef object to use as the default during construction

    type_space=19    # number of characters to allocate for type definition
    name_space=16    # number of characters to allocate for name definition
    max_comment=125  # maximum comment line length
    
    def __init__(self, name="dummy"):
        """
        sets some defaults -- if vardef.default is a vardef object, then use
        it's data as the defaults
          name = name of the variable (optional, can be reset by parse())
        """
        self.name      = name           # name of the variable
        self.raw_input = []             # list of strings used to build this vardef
        
        if (not vardef.default):
            self.shape  = ()            # shape of the variable as tuple of strings
            self.ftype  = "Real*8"      # fortran type of the variable
            self.ctype  = "double"      # C type of the variable
            self.units  = ""            # units in format "(m/s)"
            self.init_value = {}        # dictionary of strings to use to initialize the fortran variable
            self.tag_value  = {}        # dictionary of strings associated with some tag
            self.iall   = 1             # 1 if this array is allocatable (must also have nonzero
                                        # shape to produce allocatable code) -- -1 for pointer
                                        # 2 if allocatable but do not generate code for allocation/deallocation
            self.check_all = 0          # if iall!=0, then setting this will cause a test of the allocation status
                                        # before replacing the variable in the box
            self.check_size = 0         # if nonzero and this is an array then check dimension sizes before
                                        # replacing the variable in the box
            self.comment= ""            # describes this variable on one line
            self.xcomment=[]            # extra comments
            self.fstruct=""             # name of structure holding this variable, blank for global
        else:
            # set default based on stored object
            self.shape      = vardef.default.shape 
            self.ftype      = vardef.default.ftype      
            self.ctype      = vardef.default.ctype
            self.units      = vardef.default.units
            self.init_value = {}
            self.tag_value  = {}
            for x in vardef.default.init_value.keys():
                self.init_value[x] = vardef.default.init_value[x]  # copy them over
            for x in vardef.default.tag_value.keys():
                self.tag_value[x] = vardef.default.tag_value[x]  # copy them over
            self.iall       = vardef.default.iall
            self.check_all  = vardef.default.check_all
            self.check_size = vardef.default.check_size
            self.comment    = vardef.default.comment
            self.xcomment   = [ x for x in vardef.default.xcomment ]            
            self.fstruct    = vardef.default.fstruct
        
    def __str__(self):
        """
        descriptive print out
        """
        s =  "----------- vardef -----------"
        s = s + "\n name:      " + self.name + "\n"
        s = s + "\n shape:     "
        if (self.shape):
            q = "("
            for x in self.shape: q = q + x + ","
            s = s + q[:-1] + ")"
        else:
            s = s + "scalar"
        if (self.comment):    s = s + "\n comment:   " + self.comment
        for x in self.xcomment:
            s = s + "\n            " + x
        if (self.units):      s = s + "\n units:     " + self.units
        if (self.ftype):      s = s + "\n fortran:   " + self.ftype
        if (self.ctype):      s = s + "\n c/c++:     " + self.ctype
        s = s + "\n allocated: "
        if (self.iall!=0 and self.shape):
            if (self.iall==1):
                s = s + "Yes"
            elif (self.iall==-1):
                s = s + "Pointer"
            elif (self.iall==2):
                s = s + "Yes but Elsewhere"
            else:
                s = s + "Unknown"
            if (self.check_all!=0):
                s = s + " (Checked before boxing)"
        else:
            s = s + "No"
        if (self.check_size and self.shape):
            s = s + "\n size checked: Yes"
        if (self.fstruct):
            s = s + "\nFortran type: "+self.fstruct
        for x in self.init_value.keys():
            s = s + ("\n initial[%s]: %s"%(x,self.init_value[x]))
        for x in self.tag_value.keys():
            s = s + ("\n tag[%s]: %s"%(x,self.tag_value[x]))
        if (self.more_info):
            s = s + "\n"
            if (self.ftype):
                s = s + "\n fortran def: " + self.build_fortran_def()
                if (self.iall==1 or self.iall==-1 and self.shape):
                    s = s + "\n allocate:      " + self.name_fortran_allocate()
                    s = s + "\n deallocate:    " + self.name_fortran_deallocate()
                for x in self.init_value.keys():
                    d = self.build_fortran_init(x)
                    if (d): s = s + ("\n initial[%s]: %s"%(x,d))
            if (self.raw_input):
                s = s + "\n\n raw definition-->"
                for x in self.raw_input:
                    s = s + "\n    " + x
        s = s + "\n--------- end vardef ---------"
        return s

    # ------------- parsing -------------
    def parse(self, raw_input):
        """
        set attributes of this object based on the list of lines
          raw_input = list of strings defining attributes of this object,
                      there should be no blank lines or comment (#) lines
                      in this list
        """
        # --- get definition ---
        if (len(raw_input)<1): raise RuntimeError("?genvars::vardef::parse(): nothing to parse")
        m = rname.match(raw_input[0]) # get name and dimensions
        t = ()
        if (m): t = m.group(1,2)
        if (len(t)<1): raise RuntimeError("?genvars::vardef::parse(): Badly formed definition \n"
                                          + raw_input[0])

        self.name = string.strip(t[0])
        no_tick(self.name)
        if (not self.name): raise RuntimeError("?genvars::vardef::parse(): Badly formed definition \n"
                                               + raw_input[0])
        if (len(t)<2 or not t[1]):
            self.shape = ()   # scalar
        else:
            dim = string.strip(t[1])
            if (dim[0] != '(' or dim[-1] != ')'):
                raise RuntimeError("?genvars::vardef::parse(): Badly formed dimension definition \n"
                                   + raw_input[0])
            q = []            # multidimensional
            for x in string.split(dim[1:-1],','):
                q.append(string.strip(x))     # add dimensions
            self.shape = tuple(q)
            
        #
        # --- parse the rest ---
        #
        i = 1                     # next line to parse
        while(i<len(raw_input)):
            # --- get attribute, rest ---
            attribute = ""        # attribute name, e.g.  comment
            rest      = ""        # everything after comment:
            m = rattr.match(raw_input[i])
            if (m):
                t = m.group(1,2)
                attribute = t[0]
                if (len(t)>1): rest=t[1]

            if (not attribute): raise RuntimeError("?genvars::vardef::parse(): Variable " + self.name
                                                   + " has a bad attribute line\n" + raw_input[i])
            attribute = string.lower(attribute)  # convert to lower case

            # --- parse line ---
            if   (attribute == "comment"):  self.process_comment(rest)
            elif (attribute == "xcomment"): self.process_xcomment(rest)
            elif (attribute == "ftype"):    self.process_ftype(rest)
            elif (attribute == "ctype"):    self.process_ctype(rest)
            elif (attribute == "units"):    self.process_units(self.name,rest)
            elif (attribute == "init"):     self.process_init_value(self.name,rest)
            elif (attribute == "tag"):      self.process_tag_value(self.name,rest)
            elif (attribute == "allocate"): self.process_iall(rest)
            elif (attribute == "check"):    self.process_check(rest)
            elif (attribute == "fstruct"):  self.process_fstruct(self.name,rest)
            else:
                raise RuntimeError("?genvars::vardef::parse(): Unknown attribute " + attribute
                                   + " for variable " + self.name)
            i=i+1

        self.raw_input = raw_input   # save the raw input for historical value

    def process_comment(self,rest):
        """
        process a comment line
          rest = text after the attribute tag

        example,
          comment: This is a comment
        """
        self.comment = string.strip(rest)
        if (len(self.comment)>self.max_comment):
            raise RuntimeError("?genvars::vardef::comment(): Comment line length is %d exceeds maximum length of %d"
                               %(len(self.comment),self.max_comment))
        no_tick(self.comment)

    def process_xcomment(self,rest):
        """
        process a comment line
          rest = text after the attribute tag

        example,
          xcomment: This is a comment
        """
        s = string.strip(rest)
        no_tick(s)
        self.xcomment.append(s)

    def process_fstruct(self,name,rest):
        """
        process a fstruct line
          rest = text after the attribute tag

        example,
          fstruct: name
        """
        s = string.split(rest)
        if (len(s)<1 or len(s)>1):
            raise RuntimeError("?genvars::vardef::process_fstruct(): Expected a "
                               +"single fstruct name in definition " + name)
        self.fstruct = s[0]

    def process_ftype(self,rest):
        """
        process a fortran type line
          rest = text after the attribute tag
          
        example,
          ftype: Real*8
        """
        self.ftype = string.strip(rest)

    def process_ctype(self,rest):
        """
        process a C type line
          rest = text after the attribute tag

        example,
          ctype: double
        """
        self.ctype = string.strip(rest)

    def process_units(self,name,rest):
        """
        process a units line
          name = for error messages
          rest = text after the attribute tag

        example,
          units: (meter/sec)
        """
        d = string.strip(rest)
        if (d[0] != '(' or d[-1] != ')'):
            raise RuntimeError("?genvars::vardef::process_units(): Units definition in " + name
                               + " missing enclosing ():\n" + d)
        no_tick(d)
        self.units = d

    def process_init_value(self,name,rest):
        """
        process an init_value line -- the init tag is used to identify
        where this initialization is to be done.
          name = for error messages
          rest = text after the attribute tag

        example:
          init: tag  0.0
        """
        tag,value= "",""
        tv = respace.split(string.strip(rest), 1)
        if (len(tv)>0): tag=tv[0]
        if (len(tv)>1): value=tv[1]
        
        if (len(tag)==0):
            raise RuntimeError("?genvars::vardef::process_init_value(): Init line in " + name
                               + " should have the form\n"
                               + "  init:  tag  value\n"
                               + "    tag   = string used to identify where to perform initialization\n"
                               + "    value = string containing initialization value\n"
                               + "but got,\n    tag = " + tag + "\n    value = " + value)
        if (len(value)==0):
            del self.init_value[tag]
        else:
            self.init_value[tag] = value

    def process_tag_value(self,name,rest):
        """
        process a tag_value line -- the meaning of the tag depends on the usage
          name = for error messages
          rest = text after the attribute tag

        example:
          tag: mytag  Hello World
        """
        tag,value= "",""
        tv = respace.split(string.strip(rest), 1)
        if (len(tv)>0): tag=tv[0]
        if (len(tv)>1): value=tv[1]
        
        if (len(tag)==0):
            raise RuntimeError("?genvars::vardef::process_tag_value(): tag line in " + name
                               + " should have the form\n"
                               + "  tag:  mytag  value\n"
                               + "    mytag  = string identifies the tag\n"
                               + "    value = string containing tag value\n"
                               + "but got,\n    tag = " + tag + "\n    value = " + value)
        if (len(value)==0):
            del self.tag_value[tag]
        else:
            self.tag_value[tag] = value

    def process_iall(self,rest):
        """
        process an allocate line
          rest = text after the attribute tag

        example,
          allocate: Yes
        """
        rest = string.lower(string.strip(rest))
        if (rest=="pointer"):
            self.iall=-1
        if (rest=="elsewhere"):
            self.iall=2
        else:
            self.iall = truefalse(rest)

    def process_check(self,rest):
        """
        process a check line
          rest = text after the attribute tag

        example,
          check: allocate
        """
        for x in rest.split():
            s = x.strip()
            if (len(s)==0): continue
            
            s = string.lower(s)
            
            if (s=="allocate"):
                self.check_all=1
            elif (s=="noallocate"):
                self.check_all=0
            elif (s=="size"):
                self.check_size=1
            elif (s=="nosize"):
                self.check_size=0

    # ------------ building ------------
    def build_fortran_def(self):
        """
        return a string containing the fortran definition or the empty
        string if no fortran definition
        """
        if (not self.ftype): return ""    # not a fortran variable
        if (self.iall==2):   return ""    # do not define
        
        iall  = self.iall!=0 and self.shape  # true if this is allocatable
        ftype = self.ftype                   # eventually "Real, allocatable"
        dim   = self.name                    # eventually "name(dim1,dim2,...) or name(:,:,...) or name
        com   = self.comment + " " + self.units
        xcom  = ""
        for x in self.xcomment:
            sformat = "\n  %%-%ds     ! %%s"%(self.type_space+self.name_space)
            xcom += sformat%(" ",x)
        if (len(self.shape)>0):
            if (iall):
                if (self.iall>0):
                    ftype = ftype + ",allocatable"
                else:
                    ftype = ftype + ",pointer"
                q = "["
                p = "("
                for x in self.shape:
                    q=q+x+","
                    p=p+":,"
                dim = dim + p[:-1]+")"          # name(:,:,...)
                com = com + " " + q[:-1] + "]"  # add [dim1,dim2,...] to comments
            else:
                ftype = ftype
                q = "("
                for x in self.shape: q=q+x+","
                dim = dim + q[:-1]+")"          # name(dim1,dim2,...)
        if ("def" in self.init_value.keys()):
            dim += "=%s"%self.init_value['def'] # name=<def>

        sformat = "  %%-%ds :: %%-%ds ! %%s"%(self.type_space, self.name_space)
        s = sformat % (ftype, dim, com)
        if (xcom):
            s+=xcom
        return s

    def fstruct_name(self):
        """
        Return the name when part of a fortran structure (e.g. ef%point)
        """
        name = self.name
        if (self.fstruct):
            name = string.strip(self.fstruct)+"%"+name
        return name

    def build_fortran_init(self, tag):
        """
        return a string containing the fortran initial assignment or
        the empty string if no assignment corresponding to tag
          tag = string identifying the type of initialization
        """
        if (not self.ftype):                     return ""    # not a fortran variable
        if (self.iall==2):                       return ""    # defined elsewhere
        
        if (tag not in self.init_value.keys()):  return ""    # no assignment
        name = self.fstruct_name()
        n = len(name)
        format = "  %%-%ds= %%s" % (12*((n/12)+1))  # format string "%-16s", attempt readability
        return format%(name,self.init_value[tag])

    def name_fortran_allocate(self):
        """
        return a string of the name for allocating the variable or
        an empty string if no allocation is required
        """
        if (not self.ftype): return ""    # not a fortran variable
        if (self.iall==2):   return ""    # allocated elsewhere
        
        name = self.fstruct_name()
        
        if (len(self.shape)>0 and self.iall):
            dim = name
            q = "("
            for x in self.shape: q=q+x+","
            dim = dim + q[:-1]+")"          # name(dim1,dim2,...)
            return dim
        else:
            return ""
        
    def name_fortran_deallocate(self):
        """
        return a string of the name for deallocating the variable or
        an empty string if no deallocation is required
        """
        if (not self.ftype): return ""    # not a fortran variable
        if (self.iall==2):   return ""    # deallocated elsewhere

        name = self.fstruct_name()
        
        if (len(self.shape)>0 and self.iall):
            dim = name
            return dim
        else:
            return ""

    def build_box_replace(self,boxname,inew=0,axismap=None):
        """
        return a list of fortran code strings for defining this as a varcontainer variable
          boxname = name of varcontainer
          inew    = 0->use fortran variable for storage, 1->create new storage
          axismap = optional dictionary mapping axis strings to variable

        assumes the following integer variables are defined
          iopt  = replacement option
          istat = returned status of replace/remove
          ier   = returned error code
        """
        slist = []
        if (len(self.shape)>4):
            raise ValueError("can not build varcontainer variables greater than dimension 4")
        name  = self.name
        fname = self.fstruct_name()

        iall        = self.iall!=0 and self.shape     # true if this is allocatable
        icheck_all  = iall and self.check_all         # check allocation status
        icheck_size = self.shape and self.check_size  # check dimensions>0
        icheck      = icheck_all or icheck_size       # check before replacing 

        # add check
        if (icheck):
            itest = ""           # logical test string
            if (icheck_all):
                if (len(itest)>0): itest += " .and. "
                if (self.iall>0):
                    itest += "allocated(%s)"%fname
                else:
                    itest += "associated(%s)"%fname
            if (icheck_size):
                idim = ""        # dimension product such as nr*nz
                for x in self.shape:
                    if (len(idim)>0): idim+="*"
                    idim += x
                if (len(itest)>0): itest += " .and. "
                itest += idim+">0"

            slist.append("if(%s) then"%itest)
            
        # add replace
        s = "call cvc_hold(%s)\n    call replace(%s,'%s',%s,%d,iopt,istat,ier)"%(fname,boxname,name,fname,inew)
        slist.append(s)

        # add attDesc
        slist.append("if (ier==0 .and. istat/=0) then")
        units = self.units
        if (len(units)==0): units="*"
        units = clean_units(units)
        s = "  call attDesc(%s, ier, '%s', &\n'%s')"%(boxname, units, self.comment)
        slist.append(s)

        # add attAxis
        if (len(self.shape)>0 and axismap!=None):
            aname = []
            nq=0                  # maximum string length
            for a in self.shape:
                if (not a in axismap):
                    #aname=None
                    #break
                    q = "dim_"+string.strip(str(a))
                else:
                    q = axismap[a].name
                aname.append(q)
                nq = max(nq,len(q))
            if (aname):
                s = "  call attAxis(%s, (/"%boxname
                i = 0
                for a in aname:
                    if (len(a)<nq):
                        a += (nq-len(a))*' '   # pad name
                    if (i!=0): s += (",")
                    s += ("'%s'"%a)
                    i += 1
                s += ("/), ier)")
                slist.append(s)

        slist.append("end if")

        # finish off check
        if (icheck):
            slist.append("else")
            slist.append("  call remove(%s, '%s', ier)"%(boxname,name)) 
            slist.append("end if")
            
        return slist

    def build_box_remove(self,boxname):
        """
        return a list of fortran code strings for removing this from a varcontainer variable
          boxname = name of varcontainer
        """
        slist = []
        if (len(self.shape)>4):
            raise ValueError("can not build varcontainer variables greater then dimension 4")
        name  = self.name
        s = "call remove(%s,'%s',ier)"%(boxname,name)
        slist.append(s)
        
        return slist

#
# ------------------------- get_axis -----------------------------
#
def get_axis(var):
    """
    return a dictionary mapping axis dimensions to a one dimensional variable.  A vardef
    is an axis if it is one dimensional and has the 'axis' tag.
      var = list of vardef objects which will be examined
    """
    dout = {}
    for x in var:
        if (len(x.shape)==1 and 'axis' in x.tag_value):
            dout[x.shape[0]] = x
    return dout

#
# ------------------------- get_dimensions -----------------------------
#
def get_dimensions(var):
    """
    return a list of the dimensions.  
      var = list of vardef objects which will be examined
    """
    dout = {}
    for x in var:
        for y in x.shape:
            dout[y]=1
    lout = dout.keys()
    lout.sort()
    return lout

#
# ------------------------- build_box_axis_size --------------------------------
#
def build_box_axis_size(boxname, amap, isum=0):
    """
    return a list of strings which set the AxisSize and AxisType attributes for the axis variables
      boxname = box name
      amap    = dictionary mapping axis dimension to the axis variable (from get_axis())
      isum    = nonzero to add sum error test
    """
    dout = []
    for x in amap.keys():
        a = amap[x]
        if (a.tag_value.get('axis','')=='phony'):
          continue

        icheck_all  = a.iall!=0  and a.check_all      # check allocation status
        icheck_size = a.check_size                    # check dimension>0
        icheck      = icheck_all or icheck_size       # check before replacing 

        if (icheck):
            itest = ""           # logical test string
            if (icheck_all):
                if (len(itest)>0): itest += " .and. "
                if (a.iall>0):
                    itest += "allocated(%s)"%a.fstruct_name()
                else:
                    itest += "associated(%s)"%a.fstruct_name()
            if (icheck_size):
                if (len(a.shape)!=1):
                    raise RuntimeError("object is not one dimensional "+a)
                if (len(itest)>0): itest += " .and. "
                itest += "%s>0"%a.shape[0]
            dout.append("if(%s) then"%itest)
            
        dout.append("call atts(%s,'%s',ier)"%(boxname,a.name))
        dout.append("if (ier==0) call attSetInt(%s,'AxisSize',%s,ier)"%(boxname,x))
        s = a.tag_value['axis']
        if (s=='phony'):
          continue
        if (s):
            dout.append("if (ier==0) call attSetChar(%s,'AxisType','%s',ier)"%(boxname,str(s)))
        if (isum):
            dout.append("if (ier/=0) isum=isum+1")

        if (icheck):
            dout.append("end if")
             
    return dout


#
# ------------------------- build_box_axis_global --------------------------------
#
def build_box_axis_global(boxname, amap, prefix="", isum=0):
    """
    return a list of strings which set the dimension names as global attributes
      boxname = box name
      amap    = dictionary mapping axis dimension to the axis variable (from get_axis())
      prefix  = prefix to add to global names
      isum    = nonzero to add sum error test
    """
    dout = []
    dout.append("call atts(%s, ' ', ier)"%boxname)
    if (isum):
        dout.append("if (ier/=0) isum=isum+1")        
    dout.append("if (ier==0) then")
    for x in amap.keys():
        dout.append("  call attSetInt(%s,'%s',%s,ier)"%(boxname,prefix+x,x))
        if (isum):
            dout.append("  if (ier/=0) isum=isum+1")        
    dout.append("end if")
    return dout


#
# ------------------------- build_axis_fortran_def --------------------------------
#
def build_axis_fortran_def(alist):
    """
    return a list of strings which defines the axis dimensions as fortran integers
      amap = list of dimensions (from get_dimensions())
    """
    dout = []
    for x in alist:
        dout.append("  integer,private :: %s = 0"%x)
    return dout


#
# ------------------------- build_box_replace --------------------------------
#
def build_box_replace(boxname, inew, var, amap):
    """
    return a list of strings which define the box variables in fortran
      boxname = box name
      inew    = 0 if capturing a fortran variable, 1 for a new variable
      var     = list of vardef objects
      amap    = dictionary mapping axis dimension to the axis variable (from get_axis())
    """
    dout = []
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        a = x.build_box_replace(boxname, inew, amap)
        if (a): dout.append(a)
    return dout


#
# ------------------------- build_box_replace --------------------------------
#
def build_box_remove(boxname, var):
    """
    return a list of strings which remove the box variables in fortran
      boxname = box name
      var     = list of vardef objects
    """
    dout = []
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        a = x.build_box_remove(boxname)
        if (a): dout.append(a)
    return dout


#
# -------------------------- read_var_lines ----------------------------
#
def read_var_lines(file):
    """
    read in the data from the file returning a list of lists in the format

      [raw_input0, raw_input1, ...]
      raw_input? = list of strings where the first string starts with @
      
    Lines beginning with '#' are ignored.
    Blank lines are ignored.
    """
    xf = open(file, 'r')

    rvars     = []    # resulting list of lists
    raw_input = []    # individual variable definition list
    while 1:
        line = xf.readline()
        if (not line): break
        s = string.strip(line)
        if (len(s)>0):
            if (s[0] == '#'): continue
            if (s[0] == '@'):
                # -- start a new raw_input --
                if (len(raw_input)>0): rvars.append(raw_input)
                raw_input = []
            raw_input.append(s)
    if (len(raw_input)>0): rvars.append(raw_input)    # left over input
    return rvars

#
# ------------------------- build_vars --------------------------
#
def build_vars(rvars):
    """
    return a list of new vardef objects created from the rvars list of string lists.
    if a variable named "default" is found, it is not returned in the vardef list
    but is installed under vardef.default as the new default for constructing
    subsequent vardef objects.
      rvars = list of string lists as produced by read_var_lines (see above)

    e.g.
      rvars = read_var_lines("myfile.dat")
      vars  = build_vars(rvars)
      print vars[0]
      
    """
    var = []
    for x in rvars:
        m = rname.match(x[0]) # get name and dimensions
        if (x):
            name = m.group(1)
            if (name=="default"):
                vardef.default=None  # clean out old default
                #print "getting new default"
        a = vardef()
        a.parse(x)
        if (a.name == "default"):
            vardef.default = a    # new default
        else:
            var.append(a)         # new object to return
    return var

#
# ------------------------- build_fortran_def --------------------------------
#
def build_fortran_def(var):
    """
    return a list of strings which define the variables in fortran
      var = list of vardef objects which will be defined
    """
    dout = []
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        a = x.build_fortran_def()
        if (a): dout.append(a)
    return dout


#
# ------------------------- build_fortran_init --------------------------------
#
def build_fortran_init(var, tag):
    """
    return a list of strings which assign the variables in fortran
      var = list of vardef objects which will be assigned
      tag = string identifies the type of initialization
    """
    dout = []
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        a = x.build_fortran_init(tag)
        if (a): dout.append(a)
    return dout


#
# ------------------------- build_fortran_allocate --------------------------------
#
def build_fortran_allocate(var, per_line=3, stat=""):
    """
    return a list of strings which allocate the variables in fortran
      var  = list of vardef objects which will be defined
      stat = if not empty this is the name of a variable to use for a STAT test
    """
    dout = []        # list of strings to return
    if (stat):
        epar = ",STAT=%s) ; if (%s/=0) goto 100"%(stat,stat)
    else:
        epar = ")"
    # --- get max length ---
    maxl = 0
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        n = len(string.strip(x.name_fortran_allocate()))
        if (n>maxl): maxl=n
    format = "%%-%ds"%maxl   # format string, "%-32s"
    
    sum  = ""        # combined names on one line
    i = 0            # count of names added to sum
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        s = string.strip(x.name_fortran_allocate())
        if (not s): continue                 # skip empty names
        s = format%s                         # reformat
        
        if (sum): sum = sum + ","            # already one name here
        sum = sum + s
        i = i+1
        
        if (i>=per_line and sum):
            dout.append("  Allocate(%s%s"%(sum,epar))   # add new line to list
            sum = ""
            i = 0
    if (sum): dout.append("  Allocate(%s%s"%(sum,epar)) # finish up last line
    
    return dout


#
# ------------------------- build_fortran_deallocate --------------------------------
#
def build_fortran_deallocate(var, per_line=3, stat=""):
    """
    return a list of strings which deallocate the variables in fortran
      var  = list of vardef objects which will be defined
      stat = if not empty this is the name of a variable to use for a STAT test
    """
    dout = []        # list of strings to return
    if (stat):
        epar = ",STAT=%s) ; if (%s/=0) goto 100"%(stat,stat)
    else:
        epar = ")"
    # --- get max length ---
    maxl = 0
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        n = len(string.strip(x.name_fortran_deallocate()))
        if (n>maxl): maxl=n
    format = "%%-%ds"%maxl   # format string, "%-32s"
    
    sum  = ""        # combined names on one line
    i = 0            # count of names added to sum
    for x in var:
        if (x.tag_value.get('axis','')=='phony'):
          continue
        s = string.strip(x.name_fortran_deallocate())
        if (not s): continue                 # skip empty names
        s = format%s                         # reformat
        
        if (sum): sum = sum + ","            # already one name here
        sum = sum + s
        i = i+1
        
        if (i>=per_line and sum):
            dout.append("  Deallocate(%s%s"%(sum,epar))   # add new line to list
            sum = ""
            i = 0
    if (sum): dout.append("  Deallocate(%s%s"%(sum,epar)) # finish up last line
    
    return dout

#
# ------------------------- copy_to_marker --------------------------------
#
def copy_to_marker(ot, of, file, marker=""):
    """
    copy from file descriptor 'ot' to file descriptor 'of' up to
    the marker 'marker' if nonempty -- 'file' is the name of the
    file associated with 'ot' (for debugging messages)
    """
    mfound = 0
    while(1):
        line = ot.readline()
        if (not line): break             # end of file
        if (marker and re.search(marker, line)):
            mfound = 1
            break                        # marker was found
        of.write(line)
    if (marker and not mfound):
        raise RuntimeError("?genvars::copy_to_marker(): Missing marker " + marker + " in " + file)

#
# ------------------------- testme --------------------------------
#
def testme(vfile=""):
    """
    development test
      vfile = name of file containing variable definitions
            = "" -> just reprint current definitions in var list
    """
    global rvars,var

    if (vfile):
        rvars = read_var_lines(vfile)
        var   = build_vars(rvars)
        
    # ------ objects --------
    for x in var: print x,"\n"

    # --- definitions ---
    print
    for y in build_fortran_def(var):
        print y
    # --- initial values ---
    print
    for y in build_fortran_init(var,"allocate"):
        print y
    # --- allocation ---
    print
    for y in build_fortran_allocate(var):
        print y
    # --- deallocation ---
    print
    for y in build_fortran_deallocate(var):
        print y

    axis = get_axis(var)
    
    # --- box define ---
    print
    for x in build_box_replace("mybox",1,var,axis):
        for y in x:
            print y
    # --- box remove ---
    print
    for x in build_box_remove("mybox",var):
        for y in x:
            print y

    # --- box axis size ---
    print
    for x in build_box_axis_size("mybox",axis):
        print x

    # --- box axis global ---
    print
    for x in build_box_axis_global("mybox",axis):
        print x




if (__name__=="__main__"):
    if (len(sys.argv)>1):
        file = sys.argv[1]
    else:
        file = "vars.inf"
    testme(file)
