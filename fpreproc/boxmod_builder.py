#
# Code generator for building fortran variable definitions, allocation, deallocation and
# VarContainer code from a variable definition file.  See more details from help string below.
#

import getopt
import sys
import string
import os

if (os.environ.has_key('CODESYSDIR')):
  CODESYSDIR = os.environ['CODESYSDIR']
  bpython    = os.path.join(CODESYSDIR,"source","bpython")
else:
  bpython    = os.path.join("..","fpreproc")
sys.path.append(bpython)   # transp's python scripts

from genvars import *

exe = sys.argv[0]

#
# ------ input defaults -----
#
skel_name  = ""    # "myskeleton"
out_name   = ""    # "myout.f90"
var_name   = ""    # "myvar.txt"
box_name   = ""    # "mybox"
prefix     = ""    # "abc"
iview      = ""    # "iview"
ehelp      = 0     # 0 no help, 1 for help
example    = 0     # 1 to run example

#
# ------ parse command line -------
#
args = sys.argv[1:]

optlist, args = getopt.getopt(args,"i:d:o:b:p:s:xh")

if (len(args)>0):
    raise ValueError("unexpected arguments to command: "+str(args))


for a,v in optlist:
  if a=="-i":
    skel_name = v.strip()
  elif a=="-d":
    var_name = v.strip()
  elif a=="-o":
    out_name = v.strip()
  elif a=="-b":
    box_name = v.strip()
  elif a=="-p":
    prefix = v.strip()
  elif a=="-s":
    iview = v.strip()
  elif a=="-h":
    ehelp=1
  elif a=="-x":
    example = 1
  else:
    raise ValueError("unexpected option %s %s"%(a,v))

if (not example):

  # ---------------------
  # --- standard path ---
  # ---------------------
  shelp = ""   # nonempty on error

  if (len(skel_name)==0):
    shelp += "?%s: the skeleton file must be given with -i myskeleton\n"%exe
  if (len(var_name)==0):
    shelp += "?%s: the variable definition file must be given with -d deffile\n"%exe
  if (len(out_name)==0):
    shelp += "?%s: the output file must be given with -o outfile\n"%exe

  if (len(prefix)==0):
    for s in out_name:
      s = s.lower()
      if (s in string.ascii_lowercase): prefix += s
      if (len(prefix)==3): break

  if (len(prefix)==0):
    shelp += "?%s: bad prefix, use -p prefix to set a valid prefix"%exe


  if (ehelp or len(shelp)>0):
    print """
 This python script takes a variable definition file and a skeleton fortran file
 and generates fortran code which defines the fortran variables, allocates, deallocates
 and optionally boxes the data in a VarContainer.
 
 -- Usage --
 
    python  boxmod_builder.py  -i myskeleton  -d deffile -o outfile [ -b boxname -p abc ]
       -i myskeleton = defines the name of the skeleton input file
                       The input file must have a line with the marker
                         --<<<DEFINITIONS>>>--
                       where the definitions will be placed followed somewhere by a marker
                         --<<<CODE>>>--
                       where the code will be placed
       -d deffile    = file containing definitions of the variables
       -o outfile    = defines the name of the output file which will be created
       -b boxname    = box the data with VarContainer name 'boxname'
       -p abc        = name of prefix for newly created subroutines, names will be
                       abc_allocate, abc_deallocate, ...  If not given then the first
                       three letters of the output file will be used.
                       This prefix should be sufficient to provide global uniqueness.
       -s iview      = name of logical variable which will control the printing
                       of status messages
       -x            = ignore other command line arguments, generate and run on example files
                       
  -- Skeleton File Format --
    This should have the general format,

      module my_module
        use <some_module>    ! should contain the method need_mybox() when boxing data
        save                 ! save everything
        
        ... parameter,variable definitions such as defining KIND parameters ...

        --<<<DEFINITIONS>>>--

      ... more parameter,variable definitions ...
  
      contains

        ... subroutines ...

        !
        ! define a subroutine callable from outside the module
        ! suppose you had a variable dimensioned as
        ! @myvar(dim1,num)
        !
        subroutine mysetup(ier)
          dim1 = set to desired size
          num  = set to desired size

          call prefix_allocate(ier)
          if (ier/=0) return

          ... do stuff with allocated data ...
        end subroutine mysetup

        !
        ! (optional) define to reset the box variables in case something was deleted
        !
        subroutine myrebox(ier)
          ier=0
          call prefix_box_replace(ier)
          if (ier/=0) return
          call prefix_box_axis(ier)
        end subroutine
        
        --<<<CODE>>>--

        ... more subroutines ...
      end module

    The output file will contain definitions for all the variables plus the variables
       logical,private :: prefix_got    ! used to track allocation status

       for each integer dimension <dim>,
         integer,private :: <dim>       ! used to dimension arrays

         
  --- Definition File Format ---

    See $CODESYSDIR/bpython/genvars.py for more information.  The file has blocks of lines
    like the following,

    #=======================================================
    # -- make default a REAL --
    #
    @default
     ftype:   Real(kind=rspec)
     ctype:   double
     init:    allocate 0.0_rspec
  
    # -- Real definitions --
    @dtime
      comment: time stamp of last driver execution, <0 -> driver error or never run
      units:  (s)
      init:   allocate -1.0

    @amu(mx_mi)
     comment:  (D) atomic mass number of isotope
     units:    (-)
     init:     default 1.8

    @temp(mxnr_r, mx_mi)
     comment:  (D) temperature profile of isotope
     units:    (keV)

    @rhot_r(mxnr_r)
     comment: (D) normalized sqrt toroidal flux grid proportional to (Phi)**0.5
     units:   (-)
     tag:      axis Xi
    #=======================================================

    with format,

     @name                -> defines the start of a scalar
    
     @name(dim1,dim2,...) -> defines the start of an array
         dim? -> names of the dimensions
    
     @default             -> start of a template to use as the default
                             for all definitions which follow
    
     comment:  <comment line>              -> comment
     ftype:    <fortran type declaration>  -> e.g.  Real*8
     ctype:    <C type declaration>        -> e.g.  double
     units:    <units string>              -> e.g.  (keV), (-) for dimensionless
     init:     <where>  <value>            -> initialization value associated with section <where>
                                              <where>=def -> put initialization on definition line
     allocate: <logical>                   -> <logical> = "Yes","true",1 or "No","false",0 for allocation
                                              of the variable or "pointer" for pointer type
     tag:      <name>   <value>            -> general storage of name,value pairs
     fstruct   <name>                      -> this variable is part of a fortran type called <name>
  
    This builder currently ignores
      fstruct
    and recognizes the the two init sections
      init: allocate  <value>     # put initialization after allocation
      init: default   <value>     # put initialization in *_set_default subroutine
    and recognizes the tag
      tag:  axis  <value>         # specify this one dimensional variable is an axis
                                  # for its dimension with axis type <value>

  --- Example ---
  python boxmod_builder.py -x
  
  """

  if (ehelp):
    sys.exit()
    
  # for errors
  if (len(shelp)>0):
    raise ValueError("command line errors\n"+shelp)


else:


  # --------------------
  # --- example path ---
  # --------------------
  skel_name  = "myskeleton"
  out_name   = "myout_f90"   # don't want a new file with .f90 extension
  var_name   = "myvar.txt"
  box_name   = "mybox"
  prefix     = "abc"
  iview      = "iview"

  # -- create skeleton --
  eskel = open(skel_name, 'w')
  eskel.write("""
module mymodule
  use varcontainer_mod

  implicit none
  save
  integer, parameter  :: rspec = selected_real_kind(12,100)
  type(cVarContainer) :: mybox           ! variable container, may be defined in another module
  logical, private    :: ibox=.false.    ! track initialization of mybox defined in this module
  
  --<<<DEFINITIONS>>>--
contains
  !
  ! this VarContainer may be defined in another module.  This is an example
  ! of initialization that may be done in the other module.  This function is
  ! needed here.
  !
  subroutine need_mybox()
    if (.not. ibox) then
       call init_cvc(mybox)
       call mybox_fetch(mybox)  ! generated using varcontainer.py
       ibox=.true.
    end if
  end subroutine need_mybox

  !
  ! define a subroutine callable from outside the module
  !
  subroutine mysetup(ier)
    integer, intent(out) :: ier
    mx_mi  = 5
    mxnr_r = 20
    
    call abc_allocate(ier)
  end subroutine mysetup
    
  !
  ! reset the box variables in case something was deleted
  !
  subroutine myrebox(ier)
    integer, intent(out) :: ier
    ier=0
    call abc_box_replace(ier)
    if (ier/=0) return
    call abc_box_axis(ier)
  end subroutine myrebox
        
  --<<<CODE>>>--

end module
  """)
  eskel.close()

  # -- create variable definitions --
  evar = open(var_name, 'w')
  evar.write("""
#
# ------ integer definitions -----
#
@default
  ftype:   Integer
  ctype:   int
  init:    allocate 0

@nr_r
  comment: number of zones in data            
  units:    (-)

@iznum(mx_mi)
 units:    (-)
 comment:  (D) atomic number of isotope

@iview
  ftype: Logical
  ctype: int
  init:  allocate .true.
  comment: controls the display of allocation/deallocation status messages
#
# ------ Real definitions -----
#
@default
  ftype:   Real(kind=rspec)
  ctype:   double
  init:    allocate 0.0_rspec
  
@dtime
  comment: time stamp of last driver execution, <0 -> driver error or never run
  units:  (s)
  init:   allocate -1.0

@rhot_r(mxnr_r)
 comment: (D) normalized sqrt toroidal flux grid proportional to (Phi)**0.5
 units:   (-)
 tag:      axis Xi

@amu(mx_mi)
  comment:  (D) atomic mass number of isotope
  units:    (-)
  init:     default 1.8

@temp(mxnr_r, mx_mi)
  comment:  (D) temperature profile of isotope
  units:    (keV)

  """)
  evar.close()



#
# ----------------------------------------------------------------
# ---------------------------- Begin -----------------------------
# ----------------------------------------------------------------
#
isbox = len(box_name)>0

print "boxmod_builder.py: Building module variables with"
print "  skeleton file:   %s"%skel_name
print "  variable file:   %s"%var_name
print "  output   file:   %s"%out_name
if (isbox):
    print "  box name:        %s"%box_name
else:
    print "  boxing data:     No"
print "  variable prefix: %s"%prefix
print "  view control:   ",
if (iview):
    print iview
else:
    print "None"


def startWarn(f):
    f.write("  ! ========= start generated code =========\n")

def endWarn(f):
    f.write("  ! ========== end generated code ==========\n")


# -- parse variable definitions --
rvars = read_var_lines(var_name)
var   = build_vars(rvars)

# -- exclude local variable names --
bad_names = ['ier','ier2','isum','istat','iopt']   # names not allowed as variable names
for x in var:
    name = x.name.lower()
    if (name in bad_names):
        raise ValueError("the name '%s' is not allowed as a variable name"%name)

# -- open files --
fin   = open(skel_name,'r')
fout  = open(out_name,'w')

defs_marker = "--<<<DEFINITIONS>>>--"
code_marker = "--<<<CODE>>>--"

axis = get_axis(var)
dims = get_dimensions(var)

#
# --- definitions ---
# define variables and a special prefix_got variable for tracking allocation status
#
copy_to_marker(fin, fout, skel_name, defs_marker)

startWarn(fout)
for y in build_fortran_def(var):
    fout.write(y+"\n")
fout.write("\n")
for y in build_axis_fortran_def(dims):
    fout.write(y+"\n")
fout.write("\n  logical,private :: %s_got=.false.  ! .true. when variables have been allocated\n"%prefix)
fout.write("\n  private %s_allocate, %s_deallocate\n"%(prefix,prefix))
if (isbox):
  fout.write("  private %s_box_replace, %s_box_axis\n"%(prefix,prefix))
endWarn(fout)


#
# --- code ---
#
copy_to_marker(fin, fout, skel_name, code_marker)

startWarn(fout)

#
# --- allocation ---
#
fout.write("""
  !
  ! -------------- %s_allocate ------------
  ! allocate variable data and optionally put the names in the VarContainer
  ! also set values of those variables with "allocate" initialization
  !
"""%prefix) 
fout.write("  subroutine %s_allocate(ier)\n"%prefix)
fout.write("    integer, intent(out) :: ier    ! 0->ok, 1->allocation error, -1->box error\n")
fout.write("    integer              :: ier2   ! deallocate error\n\n")
fout.write("    ier=0\n")
fout.write("    if (%s_got) then\n"%prefix)
fout.write("      call %s_deallocate(ier2)  ! ignore error during deallocation\n"%prefix)
fout.write("    end if\n\n")
for y in build_fortran_allocate(var,per_line=1,stat="ier"):
    fout.write("  %s\n"%y)
for y in build_fortran_init(var,"allocate"):
    fout.write("  %s\n"%y)    # initialization after allocation
fout.write("\n    %s_got=.true.\n"%prefix)
if (isbox):
    fout.write("\n    call %s_box_replace(ier2)\n"%prefix)
    fout.write("    if (ier2/=0) ier=-1\n")
    fout.write("\n    call %s_box_axis(ier2)\n"%prefix)
    fout.write("    if (ier2/=0) ier=-1\n")
if (iview):
    fout.write("\n    if (%s) then\n"%iview)
    fout.write("      print '(a)', '%%%s_allocate: allocated variable data'\n"%prefix)
    fout.write("    end if\n")
    
fout.write("""    return\n
100 continue       ! allocation error exit
    ier=1\n""")
if (iview):
    fout.write("    if (%s) then\n"%iview)
    fout.write("      print '(a)', '?%s_allocate: error allocating variable data'\n"%prefix)
    fout.write("    end if\n")
fout.write("  end subroutine %s_allocate\n\n"%prefix)

#
# --- deallocation --
#
fout.write("""
  !
  ! -------------- %s_deallocate ------------
  ! optionally remove the data from the container and deallocate the memory
  !
"""%prefix) 
fout.write("  subroutine %s_deallocate(ier)\n"%prefix)
fout.write("    integer, intent(out) :: ier    ! 0->ok, 1->allocation error, -1->box error\n")
fout.write("    integer              :: ier2   ! box removal error\n\n")
fout.write("    ier =0\n")
fout.write("    ier2=0\n")
fout.write("    if (.not. %s_got) return\n\n"%prefix)
if (isbox):
    fout.write("    call %s_box_remove(ier2)\n\n"%prefix)
for y in build_fortran_deallocate(var,per_line=1,stat="ier"):
    fout.write("  %s\n"%y)
fout.write("\n    %s_got=.false.\n\n"%prefix)
fout.write("    if (ier==0.and.ier2/=0) ier=-1\n")
if (iview):
    fout.write("    if (%s) then\n"%iview)
    fout.write("      print '(a)', '%%%s_deallocate: deallocated variable data'\n"%prefix)
    fout.write("    end if\n")
fout.write("""    return\n
100 continue       ! deallocation error exit
    ier=1\n""")
if (iview):
    fout.write("    if (%s) then\n"%iview)
    fout.write("      print '(a)', '?%s_deallocate: error deallocating variable data'\n"%prefix)
    fout.write("    end if\n")
fout.write("  end subroutine %s_deallocate\n\n"%prefix)

#
# --- set_default ---
# build a subroutine for initializing the variables defined with the tag
#  init default  value
#
dout = build_fortran_init(var,"default")
if (dout):
    fout.write("""
  !
  ! -------------- %s_set_default ------------
  ! set values of those variables with "default" initialization
  !
"""%prefix) 
    fout.write("  subroutine %s_set_default\n"%prefix)
    for y in dout:
        fout.write("  %s\n"%y)
    fout.write("  end subroutine %s_set_default\n\n"%prefix)

#
# --- VarContainer ---
#
if (isbox):
    #
    # --- replace box variables ---
    #
    fout.write(""" 
  !
  ! -------------- %s_box_replace ------------
  ! replace the variables in the VarContainer -- called from %s_allocate
  !
"""%(prefix,prefix)) 
    fout.write("  subroutine %s_box_replace(isum)\n"%prefix)
    fout.write("""
    integer, intent(out) :: isum        ! total number of errors
    integer              :: istat, ier  ! status codes from replace
    integer              :: iopt        ! replace option

    isum = 0
    iopt = 0
    if (.not. %s_got) return
    call need_%s
    \n"""%(prefix,box_name))
    for x in build_box_replace(box_name,0,var,axis):      # 0 option => force replacement
        for y in x:
            fout.write("    %s\n"%y)
        fout.write("    if (ier/=0) isum=isum+1\n")
    if (iview):
      fout.write("\n    if (isum>0 .and. %s) then\n"%iview)
      fout.write("      print '(a,i6,a)', '?%s_box_replace: ',isum,' errors boxing variable data in %s'\n"%(prefix,box_name))
      fout.write("    end if\n")
    fout.write("  end subroutine %s_box_replace\n\n"%prefix)

    #
    # --- remove box variables ---
    #
    fout.write("""
  !
  ! -------------- %s_box_remove ------------
  ! remove variables from the VarContainer -- called from %s_deallocate
  !
"""%(prefix,prefix)) 
    fout.write("  subroutine %s_box_remove(isum)\n"%prefix)
    fout.write("    integer, intent(out) :: isum    ! nonzero on error\n")
    fout.write("    integer              :: ier     ! error flag\n\n")
    fout.write("    isum=0\n")
    fout.write("    if (.not. %s_got) return\n"%prefix)
    fout.write("    call need_%s\n\n"%box_name)
    for x in build_box_remove(box_name,var):
        for y in x:
            fout.write("    %s ; if (ier/=0) isum=isum+1\n"%y)
    if (iview):
        fout.write("\n    if (isum>0 .and. %s) then\n"%iview)
        fout.write("      print '(a,i6,a)', '?%s_box_remove: ',isum,' errors removing variable data in %s'\n"%(prefix,box_name))
        fout.write("    end if\n")
    fout.write("  end subroutine %s_box_remove\n\n"%prefix)

    #
    # --- setup box axis variables ---
    #
    fout.write(""" 
  !
  ! -------------- %s_box_axis ------------
  ! setup attributes specific to axis variables and set global
  ! variables for axis dimensions
  !
"""%prefix) 
    fout.write("  subroutine %s_box_axis(isum)\n"%prefix)
    fout.write("    integer, intent(out) :: isum    ! nonzero on error\n")
    fout.write("    integer              :: ier     ! error flag\n\n")
    fout.write("    isum=0\n")
    fout.write("    if (.not. %s_got) return\n"%prefix)
    fout.write("    call need_%s\n\n"%box_name)
    for y in build_box_axis_size(box_name,axis,isum=1):
        fout.write("    %s\n"%y)
    for y in build_box_axis_global(box_name,axis,prefix+"_",isum=1):
        fout.write("    %s\n"%y)
    if (iview):
        fout.write("    if (isum>0 .and. %s) then\n"%iview)
        fout.write("      print '(a,i6,a)', '?%s_box_axis: ',isum,' errors setting axis attributes in %s'\n"%(prefix,box_name))
        fout.write("    end if\n")
    fout.write("  end subroutine %s_box_axis\n\n"%prefix)

endWarn(fout)

#
# --- finishup ---
#
copy_to_marker(fin, fout, skel_name)

fin.close()
fout.close()


