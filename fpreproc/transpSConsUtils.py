#
# Support scripts for building TRANSP libraries and programs.  
#
# For a library, the SConscript file should contain,
#   import SConscriptTools
#   Import('*')
#   dir = env.Dir(".").srcnode().abspath   # source directory
#   SConscriptTools.defineLibrary(env,dir,Copy,distLib,transp_libraries)
#  
# The library directory must contain <name>_lib.inf. The directory may
# optionally contain the file <name>_lib.public containing the names of
# the fortran files with public modules.  Each library will add itself to
# the transp_libraries dictionary which points to the distributed library file.
#
#
# For a program, the SConscript file should contain,
#   import SConscriptTools
#   Import('*')
#   dir = env.Dir(".").srcnode().abspath   # source directory
#   SConscriptTools.defineProgram(env,dir,Copy,distExe,transp_libraries)
#
# The program directory must contain the file <name>_exe.link containing
# the names of the libraries (e.g. portlib.a, $L_NETCDF) used in the linking.  
# All of the object files will be added to the linking. This file can also contain $CPP, $FTN, 
# $FTN_MAIN to indicate whether there has a C++, fortran, fortran main dependence.  
# This supercededs the use of <name>_cpp.inf and <name>_ftn.inf files.
#
# For each TRANSP library used in linking, the library must be referenced in the
# transp_libraries dictionary.  

# ===========================================================
import os
import re
import SCons

# --- handy regular expressions ---
re_for = re.compile(r"[^\.]*[^_]\.f(|90|or)$",re.IGNORECASE) # fortran file which is not a preprocessed file
re_c   = re.compile(r"[^\.]+\.c(|pp|c)$",re.IGNORECASE)      # .c,.cc,.cpp  file 
re_lib = re.compile(r"^\s*(\w+)\.a")                         # library name ends with .a in _exe.link file
re_pub = re.compile(r"^\s*([^\s#]+)")                        # look for a file name in _lib.public file
re_comment = re.compile(r'^\s*#')                            # a comment
re_tag     = re.compile(r'^\s*\$(\w+)')                      # get the name of a tag like $CPP in _exe.link file

join = os.path.join

def defineLibrary(env, dir, distLib, dict):
    """
    Define the TRANSP library and sources for SCons.  This is expected to be
    called from an SConscript script.
      env              = environment from SConstruct
      dir              = name of the library
      distLib          = directory node where distributed library should be placed
      dict             = dictionary of additional objects to communicate through the interface
    """
    libname = os.path.basename(dir)        # directory name sets the library name
    print "  library: %s"%libname

    public_sources   = env['FORTRANEXPORT']      # list of source files with public modules, will be modified
    transp_libraries = env['TRANSP_LIBRARIES']   # map library name to installed library, will be modified
    
    ldir  = os.listdir(dir)   # list of files in the source directory   

    #
    # -- public modules in  <name>_lib.public --
    # append public source file names to public_sources list.  The environment
    # variable $FOTRANEXPORT will point to the public_sources list.
    #
    pub = libname+"_lib.public"
    hasPub = pub in ldir         # true if <name>_lib.public exists
    if hasPub:
        f = open(os.path.join(dir,pub),'r')
        while(1):
            line = f.readline()
            if (not line): break
            m = re_pub.match(line)         # look for source file name
            if (m):
                public_sources.append(m.group(1))
        f.close()
    
    # -- sources --
    fsrc  = filter(re_for.match, ldir)     # fortran files
    csrc  = filter(re_c.match,   ldir)     # C files
    cobj  = []
    fobj  = []
    
    # -- building --
    if (csrc):
        cobj = env.StaticTranspC(csrc)
    
    if (fsrc):    
        fobj = env.StaticTranspFortran(fsrc)
 
    obj = cobj + filter_objects(env, fobj)  # filter out the fortran preprocessed source files *_.f

    lib = env.StaticLibrary(target=libname,source=obj)  # define library

    # -- install --
    dlib = os.path.join(distLib.abspath,str(lib[0]))
    env.Command(dlib,lib,SCons.Defaults.Copy('$TARGET','$SOURCE'))  # copy from build/<name>/<name>.a to dist/lib/<name>.a

    transp_libraries[libname] = dlib     # associate the distributed library with the library name



def defineProgram(env, dir, distExe, dict):
    """
    Define the TRANSP program and sources for SCons.  This is expected to be
    called from an SConscript script.
      env              = environment from SConstruct
      dir              = name of the library
      distExe          = directory node where distributed program should be placed
      dict             = dictionary of additional data to communicate through interface
    """
    execname = os.path.basename(dir)       # directory name sets the executable name
    print "  program: %s"%execname

    flink = execname+"_exe.link"
    ldir  = os.listdir(dir)

    if (flink not in ldir):
        raise RuntimeError("did not find %s in %s"%(flink,dir))
    flink = env.File(flink,dir)            # make .link file a File

    transp_libraries = env['TRANSP_LIBRARIES'] # map library name to installed library, will be modified
    
    # -- libraries --
    libs = []
    has_ftn      = 0    # true if $FTN      in <name>_exe.link
    has_ftn_main = 0    # true if $FTN_MAIN in <name>_exe.link
    has_cpp      = 0    # true if $CPP      in <name>_exe.link
    f = open(flink.abspath,'r')
    for x in f.readlines():          # look for <lib>.a lines in <name>_exe.link file
        if (re_comment.match(x)):
            continue
        m = re_lib.match(x)
        if (m):
            xname = m.group(1)       # base name of library, e.g. 'portlib', must have already been created with makeLibrary()
            if (not transp_libraries.has_key(xname)):
                raise RuntimeError("the program '%s' references the unknown TRANSP library '%s'"%(execname,xname))
            libs.append(transp_libraries[xname])
        else:
            m = re_tag.match(x)
            if (m):
                name = m.group(1).upper()
                if (name == "CPP"):
                    has_cpp = 1
                elif (name == "FTN"):
                    has_ftn = 1
                elif (name == "FTN_MAIN"):
                    has_ftn_main = 1

    f.close()

    # -- sources --
    fsrc  = filter(re_for.match, ldir)     # fortran files
    csrc  = filter(re_c.match,   ldir)     # C files
    cobj  = []
    fobj  = []

    # -- building --
    if (csrc):
        cobj = env.StaticTranspC(csrc)
    
    if (fsrc):    
        fobj = env.StaticTranspFortran(fsrc)
 
    obj = cobj + filter_objects(env,fobj)  # filter out the fortran preprocessed source files *_.f

    # -- select linker --
    transp_links = env.get('TRANSP_LINKS',None)   # dictionary mapping executable name to linktype ("fortran", "fortranc++", "c++", "c")
    if (transp_links and transp_links.contains(exe)):
        linktype = transp_links[exe]
    else:
        ftnmain = (execname+".for" in fsrc) or (execname+".f90" in fsrc)
        cxxmain = (execname+".cpp" in csrc) or (execname+".cc" in csrc)

        if (ftnmain):
            # if $FORTRANCPP is defined and nonzero,
            #   FORTRANCPP=1 -> link all fortran executables which contain c++ with C++ linker
            #   FORTRANCPP=2 -> link all fortran executables with C++ linker
            fortrancpp = env.get('FORTRANCPP',0)
            try:
                fortrancpp = int(fortrancpp)
            except ValueError, e:
                fortrancpp = 0
            
            if (fortrancpp==2 or (has_cpp and (has_ftn_main or fortrancpp==1))):
                linktype = "fortranc++"
            else:
                linktype = "fortran"
        elif (has_cpp or cxxmain):
            linktype = "c++"
        else:
            linktype = "c"

    # -- link --
    source = obj+libs+[flink]
    if (linktype == "fortran"):
        exe = env.ProgramTranspFortran(target=execname,source=source)
    elif (linktype == "fortranc++"):
        exe = env.ProgramTranspFortranCPP(target=execname,source=source)
    elif (linktype == "c++"):
        exe = env.ProgramTranspCPP(target=execname,source=source)
    else:
        exe = env.ProgramTranspCC(target=execname,source=source) 

    # --- install ---
    dexe = os.path.join(distExe.abspath,str(exe[0]))
    env.Command(dexe,exe,SCons.Defaults.Copy('$TARGET','$SOURCE'))     # copy from build/<name> to dist/exe/<name>



def defineDebugLibrary(env, dir, build_dir, dict):
    """
    Definitions for a debug TRANSP library.
      env              = environment from SConstruct
      dir              = name of the library
      build_dir        = build directory
      dict             = dictionary with additional data
    """
    libname = os.path.basename(dir)        # directory name sets the library name
    print "  debug library: %s"%libname

    debug_files = dict['debug_files']

    public_sources   = env['FORTRANEXPORT']    # list of source files with public modules, will be modified
    transp_libraries = env['TRANSP_LIBRARIES'] # map library name to installed library, will be modified

    if (not transp_libraries.has_key(libname)):
        raise RuntimeError("attempt to build debug library from nonexistent library "+libname)

    ldir = os.listdir(dir)   # list of files in the source directory   

    #
    # -- public modules in  <name>_lib.public --
    # append public source file names to public_sources list.  The environment
    # variable $FOTRANEXPORT will point to the public_sources list.
    #
    pub = libname+"_lib.public"
    hasPub = pub in ldir         # true if <name>_lib.public exists
    if hasPub:
        f = open(os.path.join(dir,pub),'r')
        while(1):
            line = f.readline()
            if (not line): break
            m = re_pub.match(line)         # look for source file name
            if (m):
                public_sources.append(m.group(1))
        f.close()

    # -- sources --
    fsrc  = filter(re_for.match, ldir)     # fortran files
    csrc  = filter(re_c.match,   ldir)     # C files
    cobj  = []
    fobj  = []

    if (debug_files != None):
        tsrc = [ x for x in fsrc if debug_files.has_key(x) ]
        fsrc = tsrc
        tsrc = [ x for x in csrc if debug_files.has_key(x) ]
        csrc = tsrc        
                    
    # -- use source in debug directory if it exists --
    build_dir = env.Dir("#"+build_dir).abspath
    
    bdir = {}
    for x in os.listdir(build_dir):
        bdir[x] = 1
    for i in range(len(fsrc)):
        f = fsrc[i]
        if (f in bdir):
            fsrc[i] = join(build_dir,f)
            print "  --> using %s from debug directory"%f
    for i in range(len(csrc)):
        c = csrc[i]
        if (c in bdir):
            csrc[i] = join(build_dir,c)
            print "  --> using %s from debug directory"%c
    
    # -- building --
    if (csrc):
        cobj = env.StaticTranspDebugC(csrc)
    
    if (fsrc):    
        fobj = env.StaticTranspDebugFortran(fsrc)
 
    obj = cobj + filter_objects(env, fobj)  # filter out the fortran preprocessed source files *_.f

    lib = env.TranspDebugStaticLibrary(target=libname,source=obj)  # define library

    env.Depends(lib,transp_libraries[libname])  # make sure nondebug library is built


def loadIfNone(env, name, tool):
    """
    Load in a tool if env["name"] does not exist.  Return true if name is loaded.
      env      = environment being modified
      name     = name of variable in the environment which will be created by the tool
      tool     = name or list of tool names to search
    """
    if (env.has_key(name)):
        return 1
    toolpath = env.get('ToolPath',"")
    for u in env.Split(tool):
        t = SCons.Tool.Tool(u,toolpath)   # tool file should be somewhere
        if (t.exists(env)):
            t(env)             # add to environment
            if (not env.has_key(name)):
                print env
                raise AttributeError("loaded tool %s but environment vairable %s was not created"%(u,name))
            return 1
    return 0

            
def findDirectory(env, name, path, checkdir=None, type=0):
    """
    Look for a library or include file in a list of directories.
    If found return the tuple (dir,fnam) of the directory and file found.
      env = environment of search
      name = base name for search
      path = list of directories for search
      checkdir = if defined search this directory first, can be a space delimited set of directories
      type     = 0 -> look for file name as is
               = 1 -> look for static library file using name as the base name
               = 2 -> look for shared library file using name as the base name
               = 3 -> look for static and shared library files
    """
    if (type==0):
        sname = [name]   # list of names to search for
    else:
        sname = []
        if (type==1 or type==3):
            sname.append(env.subst('${LIBPREFIX}%s${LIBSUFFIX}'%name))
        if (type==2 or type==3):
            sname.append(env.subst('${SHLIBPREFIX}%s${SHLIBSUFFIX}'%name))
            
    if (len(sname)==0):
        raise ValueError("bad type argument = "+str(type))

    if (checkdir!=None):
        checkdir = env.Split(checkdir)
        for dir in checkdir:
            for libname in sname:
                if (os.path.isfile(os.path.join(dir,libname))):
                    return (os.path.normpath(dir),libname)
            
    path = env.Flatten(env.Split(path))
    #print "path=",path
    #print "sname=",sname
    for dir in path:
        for libname in sname:
            if (os.path.isfile(os.path.join(dir,libname))):
                return (os.path.normpath(dir),libname)
    return None


def filter_objects(env, objs):
    """
    Return only the objects which are Object files.
      env  - environment
      objs - list of File objects     
    """
    re_obj = re.compile(r"\.["+env['OBJSUFFIX']+r"]$",re.IGNORECASE) # an object file
    fobjs  = filter(lambda x: re_obj.search(str(x)), objs)

    return fobjs

