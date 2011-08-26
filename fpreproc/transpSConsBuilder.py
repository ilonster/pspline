#
# This is a tool which provides the Builders,Scanners and Emitters for building
# TRANSP code using SCons.  This means using a preprocessor when building from
# fortran files and providing the flexibility to link with the Fortran, C++ or
# C linker.  Fortran files are assumed to be in free source form when they have
# the .f90 extension.
#
# The SConstruct file should add $CODESYSDIR/source/bpython to sys.path.
#
# -- builders --
# StaticTranspFortran, SharedTranspFortran - standard fortran compilers
# StaticTranspDebugFortran, SharedTranspDebugFortran - compiles with debug flags
#
# StaticTranspC, SharedTranspC - compiles C and C++, not muc different then SCons default
# StaticTranspDebugC, SharedTranspDebugC - compiles with debug flags
#
# ProgramTranspFortran - link with the fortran compiler
# ProgramTranspCPP - link with the C++ compiler
# ProgramTranspFortranCPP - link fortran executable with C++ compiler, useful for forcing
#                           the link of the fortran executable when FTN_MAIN is not in the
#                           <name>_exe.link file
# ProgramTranspCC - link with the C compiler
#
# The object files, local (TRANSP) libraries which are a part of this build and
# the <name>_exe.link file if it exists  should be the sources in the ProgramTransp* builders.
#
# The following lines in the <name>_exe.link file will modify the linking,
#   $L_<exe>  - add an external library as one of the libraries to be linked with.
#               See the external_packages python variable below for the available libraries.
#   $FTN_MAIN - indicates that this executable contains a fortran main program
#   $FTN      - indicates that this executable code will contain fortran code and so needs
#               to be linked with the fortran libraries (see FTN_MAIN,FTN_TRAILER environment variables)
#   $CPP      - indicates that this executable code will contain C++ code and so needs
#               to be linked with the C++ libraries (see CPP_TRAILER environment variable)
#
#   everything else including the object file names will be ignored in this file
#
# -- env definitions --
# $CODESYS_DEBUG       = set this to an integer XYZ to see debug messages.  Each digit turns on a different section
#                          X -> messages during emitting phase when the object and module files produced are identified
#                          Y -> messages during scanning phase when implicit dependencies are identified
#                          Z -> messages during compile phase
#                        So you can set this to 101 to turn on emitting and compiling messages.
#
# $LINKLOAD_DEBUG      = if a nonzero integer then linking debug messages will be written
#
# $FORTRAN             = fortran command (e.g. lf95)
# $FORTRANFLAGS        = flags for fortran compiling
# $FORTRANFLAGS_DEBUG  = flags for fortran debug compiling (replaces $FORTRANFLAGS)  
# $FORTRAN_FREE        = additional flags added when compiling free format code
# $FORTRAN_FIXED       = additional flags added when compiling fixed format code
# $FORTRAN_FREE_EXT    = extension to use for preprocessed free formated code ["f90"]
# $FORTRAN_FIXED_EXT   = extension to use for preprocessed fixed formated code ["for"]
# $FORTRAN_EXPORT      = this should be a list which contains the names of all files exporting public modules
# $FORTRAN_SHARE       = additional flag added to shared fortran compile
#                        it will be setup for you in SConstruct
# $FORTRANPUBLIC       = should point to the directory where public modules will be placed
# $FORTRANMODSUFFIX    = suffix for modules ["mod"]
# $FORTRANPATH         = paths used for searching for include and module files
# $FORTRANMODDIRPREFIX = see SCons manual
# $FORTRANMODDIR       = directory where module files will be placed, generally this should be kept empty
# $FORTRANMODDIRSUFFIX = see SCons manual
# $FPPDEFINES          = preprocessor flags for fortran
# $FPPPATH             = paths used during preprocessing
#
# $CC                  = C compiler command
# $CCFLAGS             = usual definition used for distributed C code
# $CCFLAGS_DEBUG       = CCFLAGS when compiling for C debugging, this replaces $CCFLAGS
# $CPPPATH             = see SCons manual
# $CPPDEFINES          = see SCons manual
#
# $CXX                 = C++ compiler command
# $CXXFLAGS            = usual definition used for distributed C++ code
# $CXXFLAGS_DEBUG      = CXXFLAGS when compiling for C++ debugging, this replaces $CXXFLAGS
#
# $CPP_TRAILER         = trailer added during fortran linking when C++ object files are included
# $FTN_TRAILER         = trailer added during C,C++ linking when fortran object files are included
# $FTN_MAIN            = should point to the system object file containing the fortran main()
#
#      These trailers can be set automatically by the tools trscons_g++ and trscons_lf95.
#
# $LINKFLAGS           = these flags are added for all linking
# $LINK_FORTRAN        = the command for fortran linking [$FORTRAN,"gfortran"]
# $LINK_FORTRAN_TRAILER= an additional string added to the fortran link line 
# $LINK_FORTRANFLAGS   = flags applied during fortran linking only
# $LINK_CXXFLAGS       = flags applied during C++ linking only
# $LINK_CXX            = command for C++ linking [$CXX,"g++"]
# $LINK_CXX_TRAILER    = an additional string added to the C++ link line 
# $LINK_CC             = command for C linking [$CC,"gcc"]
# $LINK_CCFLAGS        = flags applied during C linking only
# $LINK_CC_TRAILER     = an additional string added to the C link line
#
# $L_???               = for each of the external libraries these variables should be the list of libraries to be
#                        added onto the link line.  Also setup LIBPATH to point to the library directories.
#                        See the external_packages variable for the packages which are supported.  The purpose
#                        of these variables is to customize the library dependencies of each individual program.
#                        To use one of these libraries during linking you must setup the <name>_exe.link file.
#
#        examples,
#                  L_FFTW="fftw",L_LAPACK=["lapack","blas"],L_SUPERLU=["superlu","blas"]
#                  TRLIB_PATH=["/usr/local/lff95/lib", "/usr/local/lib", "/usr/local/mdsplus/lib", "/usr/local/superlu/lib"]
#
#        these can also be set by the various trscons_??? tools though you should make sure the
#        trscons_path tool is loaded first to setup the search paths
#
#
# =====================================================================================
# ---- import ----
import os
import sys
import re
import shutil
import string

import SCons.Scanner
import SCons.Scanner.Fortran
import SCons.Tool.fortran

join = os.path.join

#
# transp paths and imports, the sys.path should have been extended
# to include $CODESYSDIR/source/bpython in the SConstruct file.
#

from fpp import *

external_packages = ['BLAS','NETCDF', 'MDSPLUS', 'RFFTW', 'FFTW', 'LAPACK', 'SUPERLU']  # used for L_<name> link definitions

transpFortranSuffixes = [".f90",".for",".f",".F","F90",".FOR"]
transpFortranFreeExts = ["f90"]   # extensions recognized as free format (lower case only)

transpCSuffixes   = [".c"]
transpCXXSuffixes = ['.cpp', '.cc', '.cxx', '.c++', '.C++']

if SCons.Util.case_sensitive_suffixes('.c', '.C'):
    transpCXXSuffixes.append('.C')
else:
    transpCSuffixes.append('.C')

# ----------------- Fortran -------------------
re_base = re.compile(r"^([^\.]+)\.([^\.]+)$")    # split off extension

# SCons formatted line for fortran building

class TranspFortran:
    """
    Allows construction of actions with are modified by the member data.
    """
    def __init__(self, isdebug=0, isshare=0):
        """
        isdebug = nonzero to debug compile
        """
        self.isdebug = isdebug   # true for debug compiling
        self.isshare = isshare   # true for share compiling
        
        self.freeCom  = '$FORTRAN_FREECOM'
        self.fixedCom = '$FORTRAN_FIXEDCOM'

        self.freeComDebug  = '$FORTRAN_FREECOM_DEBUG'
        self.fixedComDebug = '$FORTRAN_FIXEDCOM_DEBUG'
                
        self.freeComSh  = '$FORTRAN_FREECOM_SH'
        self.fixedComSh = '$FORTRAN_FIXEDCOM_SH'

        self.freeComDebugSh  = '$FORTRAN_FREECOM_DEBUG_SH'
        self.fixedComDebugSh = '$FORTRAN_FIXEDCOM_DEBUG_SH'
                
    def transpFortranActionFunction(self,target,source,env):
        """
        An action which preprocesses the fortran source then compiles it.
        """
        ta = target[0].abspath                       # target .o file
        p  = os.path.dirname(ta)                     # path to build directory
        rn = source[0]                               # source node
        ra = rn.abspath                              # full path to source file
        r  = os.path.basename(ra)                    # the .for or .f90 file name
        base,tail =  re_base.match(r).groups()       # ("mysource","f90")
        tail = tail.lower()                          # "f90"
        free = tail in transpFortranFreeExts         # true if free format fortran source

        info = int(env.get("CODESYS_DEBUG","0"))%10 >0         # true to print out info during each step

        fpppath = env.subst('$_FPPINCFLAGS',target=target)     # -I flags for fortran preprocessing
        fppdefs = env.subst('$_FORTRANDEFFLAGS')               # defines for fortran preprocessing

        if (free):
            ext = env.get("FORTRAN_FREE_EXT", "f90")
            maxlen = 132
        else:
            ext = env.get("FORTRAN_FIXED_EXT", "for")
            maxlen = 72
        
        s = os.path.join(p,base+"_."+ext)            # the preprocessed file
        z = s+"_tmp"                                 # intermediate step in preprocessing

        if (info):
            print "transpFortranAction: %s"%ra
            print "   -> processed source:  %s"%s
            for x in target:
                print "   -> target:            %s"%str(x)
            print "   -> temporary file:    %s"%z

        #
        # -- modules --
        # identify generated module files and delete them before building
        #
        isExport = r in env.Split(env.get('FORTRANEXPORT',[]))   # true to export generated modules,
                                                                 # look for file.f90 in environment list
        if (isExport):
            fpub = env.Dir(env.subst('$FORTRANPUBLIC')).abspath  # public directory for exported modules
            if (not os.path.exists(fpub)):
                os.mkdir(fpub)

        mods = []                              # module files created by this command (file_base_name, abs_path)
        mext = string.strip(env.subst('$FORTRANMODSUFFIX'))
        re_mext = re.compile(r'.*'+mext+r'$')
        for x in target:                       # look in target list for the generated modules
            q = x.abspath
            if (re_mext.match(q)):
                bmod = os.path.basename(q)     # mymodule.mod
                mods.append((bmod,q))
                # - clear out old modules -
                if (os.path.isfile(bmod)):
                    os.unlink(bmod)            # remove module file in current directory
                if (os.path.isfile(q)):
                    os.unlink(q)               # remove module file in final destination
                if (isExport):
                    qp = os.path.join(fpub,bmod)
                    if (os.path.isfile(qp)):
                        os.unlink(qp)          # remove publicly exported module file

        if (len(mods)!=0 and info):
            print "   -> generated modules:",[ x[0] for x in mods ]
            if (isExport):
                print "   -> export modules to: %s"%fpub

        # -- pre-preprocess --
        if (os.path.isfile(s)):
            os.unlink(s)                   # remove previous preprocessed file

        f = open(ra,'r')
        slines = f.readlines()             # grab source in list of lines
        f.close()

        pre = prefpp()
        out = pre(slines, free=free, maxlen=maxlen)
        f = open(z,'w')
        try:
            for x in out:
                f.write(x+'\n')
            f.close()
        
            # -- preprocess through gcc --
            fcmd = "gcc -E -P %s %s -x c %s"%(fppdefs,fpppath,z)
            if (info):
                print "   -> preprocess: ",fcmd
            f = os.popen(fcmd,'r')                   # read output from stdout of gcc
            gout = f.readlines()                     # grab lines of preprocessed output
            #for x in gout:
            #    print x,
            f.close()
        finally:
            if (os.path.isfile(z)):
                os.unlink(z)                         # clean up intermediate file

        # -- post-preprocess --
        post = postfpp()
        fout = post(gout, free=free, maxlen=maxlen)
        f = open(s,'w')
        for x in fout:
            f.write(x+'\n')      # write preprocessed source file
        f.close()

        # -- compile --
        com = self.getCom(env, free, s, target)
        
        if (info):
            print "   -> free    = %d"%free
            print "   -> ext     = %s"%ext
            print "   -> command = %s"%com
        
        stat = os.system(com)

        #
        # -- finish off modules --
        #
        for m,a in mods:
            if (stat==0):
                if (info):
                    print "renaming %s to %s "%(m,a)
                os.rename(m,a)                                # move module file to final resting place
                #if (isExport):
                #    shutil.copyfile(a,os.path.join(fpub,m))   # copy to public place
            else:
                if (os.path.isfile(m)):
                    os.unlink(m)           # remove module file in current directory on a compile error

        return stat

    def getCom(self, env, free, s, target):
        """
        get the com string
          self   = self
          env    = environment
          free   = true if free source format
          s      = source file name
          target = targets
        """
        if (self.isshare):
            if (free):
                if (self.isdebug):
                    com = env.subst(self.freeComDebugSh,source=env.File(s),target=target)
                else:
                    com = env.subst(self.freeComSh,source=env.File(s),target=target)
            else:
                if (self.isdebug):
                    com = env.subst(self.fixedComDebugSh,source=env.File(s),target=target)
                else:
                    com = env.subst(self.fixedComSh,source=env.File(s),target=target)        
        else:
            if (free):
                if (self.isdebug):
                    com = env.subst(self.freeComDebug,source=env.File(s),target=target)
                else:
                    com = env.subst(self.freeCom,source=env.File(s),target=target)
            else:
                if (self.isdebug):
                    com = env.subst(self.fixedComDebug,source=env.File(s),target=target)
                else:
                    com = env.subst(self.fixedCom,source=env.File(s),target=target)
        return com
    

    def transpFortranActionDesc(self,target,source,env):
        """
        Describes the fortran compilation.  Used by SCons when printing out the action.
        """
        ta = target[0].abspath                       # target .o file
        p  = os.path.dirname(ta)                     # path to build directory
        rn = source[0]                               # source node
        ra = rn.abspath                              # full path to source file
        r  = os.path.basename(ra)                    # the .for or .f90 file name
        base,tail =  re_base.match(r).groups()       # ("mysource","f90")
        tail = tail.lower()
        free = tail in transpFortranFreeExts

        fpppath = env.subst('$_FPPINCFLAGS',target=target)     # -I flags for fortran preprocessing
        fppdefs = env.subst('$_FORTRANDEFFLAGS')               # defines for fortran preprocessing

        if (free):
            ext = env.get("FORTRAN_FREE_EXT", "f90")
            maxlen = 132
        else:
            ext = env.get("FORTRAN_FIXED_EXT", "for")
            maxlen = 72
        
        s = os.path.join(p,base+"_."+ext)            # the preprocessed file

        com = self.getCom(env, free, s, target)
        return com

transpFortranOpt       = TranspFortran(0,0)
transpFortranOptAction = SCons.Action.Action(transpFortranOpt.transpFortranActionFunction,
                                             transpFortranOpt.transpFortranActionDesc)

transpFortranDebug       = TranspFortran(1,0)
transpFortranDebugAction = SCons.Action.Action(transpFortranDebug.transpFortranActionFunction,
                                               transpFortranDebug.transpFortranActionDesc)

transpFortranOptSh       = TranspFortran(0,1)
transpFortranOptActionSh = SCons.Action.Action(transpFortranOptSh.transpFortranActionFunction,
                                             transpFortranOptSh.transpFortranActionDesc)

transpFortranDebugSh       = TranspFortran(1,1)
transpFortranDebugActionSh = SCons.Action.Action(transpFortranDebugSh.transpFortranActionFunction,
                                               transpFortranDebugSh.transpFortranActionDesc)

#
# --- emitter ---
# adds the module .mod files to the targets produced from the source.  This is
# snatched from the scons Tool/fortran.py file and customized to set the
# module directory
#
def _fortranEmitter(target, source, env):
    node = source[0].rfile()
    if not node.exists() and not node.is_derived():
       print "Could not locate " + str(node.name)
       return ([], [])
    mod_regex = """(?i)^\s*MODULE\s+(?!PROCEDURE)(\w+)"""
    cre = re.compile(mod_regex,re.M)
    # Retrieve all USE'd module names
    modules = cre.findall(node.get_contents())
    # Remove unique items from the list
    modules = SCons.Util.unique(modules)
    # Convert module name to a .mod filename
    suffix = env.subst('$FORTRANMODSUFFIX')
    moddir = env.subst('$FORTRANMODDIR')
    modules = map(lambda x, s=suffix: string.lower(x) + s, modules)
    if (len(modules)>0):
        isExport =  os.path.basename(node.path) in env.Split(env.get('FORTRANEXPORT',[]))
        if (isExport):
            # generated module is moved to public area
            moddir = env.Dir(env.subst('$FORTRANPUBLIC')).abspath  # public directory for exported modules
    for m in modules:
       target.append(env.fs.File(m, moddir))
    return (target, source)

# -- snatched code --
def FortranEmitter(target, source, env):
    target, source = _fortranEmitter(target, source, env)
    return SCons.Defaults.StaticObjectEmitter(target, source, env)

def ShFortranEmitter(target, source, env):
    target, source = _fortranEmitter(target, source, env)
    return SCons.Defaults.SharedObjectEmitter(target, source, env)

#
# -- more snatched code --
# adds preprocessed file to the list of emitted code
#
class transpFortranEmitterClass:
    def __init__(self, isshare=0):
        self.isshare = isshare   # true if for a shared library

    def emitter(self,target,source,env):
        if (self.isshare):
            t = ShFortranEmitter(target, source, env)
        else:
            t = FortranEmitter(target, source, env)
            
        if (1):
            #
            # this section will add the preprocessed file as one of the targets
            # of the fortran build.  If you use this then you will need to filter
            # these files out of the env.Object() result so they do not end up
            # being put inside the library.  An advantage of using this is that
            # the preprocessed file will be cleaned up along with the object file.
            #
            ta = target[0].abspath                       # target .o file
            p  = os.path.dirname(ta)                     # path to build directory
            rn = source[0]                               # source node
            ra = rn.abspath                              # full path to source file
            r  = os.path.basename(ra)                    # the .for or .f90 file name
            base,tail =  re_base.match(r).groups()       # ("mysource","f90")
            tail = tail.lower()
            free = tail in transpFortranFreeExts
    
            if (free):
                ext = env.get("FORTRAN_FREE_EXT", "f90")
            else:
                ext = env.get("FORTRAN_FIXED_EXT", "for")
        
            s = os.path.join(p,base+"_."+ext)            # the preprocessed file
        
            t[0].append(env.File(s))
            #t[0][:] = [ os.path.basename(str(s)) for s in t[0] ]
        

        info = int(env.get("CODESYS_DEBUG","0"))/100 >0   # true to print out info
        if (info):
            s = [str(x) for x in source]
            u = [str(x) for x in t[0]]
            print "from %s emitting %s"%(str(s),str(u))
        return t

transpFortranEmitter   = transpFortranEmitterClass(0).emitter
transpShFortranEmitter = transpFortranEmitterClass(1).emitter

# --- regular expressions ---
include_regex = """(?i)(?:^|['">]\s*;)\s*INCLUDE\s+(?:\w+_)?[<"'](.+?)(?=["'>])"""

use_regex = "(?i)(?:^|;)\s*USE(?:\s+|(?:(?:\s*,\s*(?:NON_)?INTRINSIC)?\s*::))\s*(\w+)"

cinclude_regex = '^[ \t]*#[ \t]*(?:include|import)[ \t]*[<"]([^>"]+)[>"]'

def_regex = """(?i)^\s*MODULE\s+(?!PROCEDURE)(\w+)"""

cre_incl    = re.compile(include_regex, re.M)              # fortran INCLUDE lines
cre_use     = re.compile(use_regex, re.M)                  # fortran USE lines
cre_include = re.compile(cinclude_regex, re.M)             # fortran #include lines
cre_def     = re.compile(def_regex, re.M)

#
# --- scanning ---
# Snatched from scons Scanner/Fortran.py file.  Also need to scan for
# #include preprocessor lines and throw out self usages of module files.
#
def transpFortranScannerFunction(node,env,path):
    """
    Scan a fortran file for USE, INCLUDE and cpp #include statements.  This code was pulled
    from scon's Fortran.py and C.py files
    """
    info = (int(env.get("CODESYS_DEBUG","0"))%100)/10 >0   # true to print out info
    if (info):
        print "scanning ",str(node)
    node = node.rfile()       
    if not node.exists():
        return []

    # cache the includes list in node so we only scan it once:
    if node.includes != None:
        mods_and_includes = node.includes
    else:
        r  = os.path.basename(node.abspath)                    # the base file name
        mext = env.subst('$FORTRANMODSUFFIX').strip()    
        re_mext = re.compile(r'.*'+mext+r'$')                  # don't match .mod files

        if (not re_mext.match(r)):
            # retrieve all included filenames
            includes = cre_incl.findall(node.get_contents())
        
            # retrieve all USE'd module names
            modules = cre_use.findall(node.get_contents())
        
            # retrieve all #included module names
            cincludes = cre_include.findall(node.get_contents())

            # retrieve all defined module names
            defmodules = cre_def.findall(node.get_contents())

            # Remove all USE'd module names that are defined in the same file
            d = {}
            for m in defmodules:
                d[m] = 1
            modules = filter(lambda m, d=d: not d.has_key(m), modules)

            # Convert module name to a .mod filename
            if env.has_key('FORTRANMODSUFFIX'):
                suffix = env.subst('$FORTRANMODSUFFIX')
            else:
                suffix = ".mod"

            modules = map(lambda x, s=suffix: x.lower() + s, modules)

            if (modules):
                # remove any modules from the dependency which are exported by the file
                # to prevent a dependency loop.
                t = SCons.Tool.fortran.FortranEmitter([], [node], env)
                mx = [ ]                       # modules exported by this file by name
                for x in t[0]:
                    xa = x.abspath             # full path to source file
                    xb = os.path.basename(xa)  # the .for or .f90 file name
                    mx.append(xb)
                m = {}                         # use as a set to hold dependent module names
                for x in modules:
                    if (x not in mx):
                        m[x] = 1               # keep the module if not exported by the file
                modules = m.keys()             # unique list of modules not exported by file

            # Remove unique items from the list
            mods_and_includes = SCons.Util.unique(includes+modules+cincludes)

            if (info):
                print "    -->deps = ",mods_and_includes
        else:
            mods_and_includes = []
            if (info):
                print "    -- no scan --"
        
        node.includes = mods_and_includes
        
    # This is a hand-coded DSU (decorate-sort-undecorate, or
    # Schwartzian transform) pattern.  The sort key is the raw name
    # of the file as specifed on the USE or INCLUDE line, which lets
    # us keep the sort order constant regardless of whether the file
    # is actually found in a Repository or locally.
    nodes = []
    source_dir = node.get_dir()
    if (callable(path)):
        path = path()
    all_path   = (source_dir,) + tuple(path)
    for dep in mods_and_includes:
        n = SCons.Node.FS.find_file(dep, all_path,info)
        if n is None:
            pout = "No dependency generated for file: %s (referenced by: %s, path: %s) -- file not found" % (dep, node, all_path)
            SCons.Warnings.warn(SCons.Warnings.DependencyWarning,pout)
        else:
            sortkey = dep
            nodes.append((sortkey, n))

    nodes.sort()
    nodes = map(lambda pair: pair[1], nodes)
    return nodes


def scan_check(node,env):
    #print "calling scan_check for ",str(node)
    c = not node.has_builder() or node.current(env.get_calculator())
    return c

transpFortranScanner = SCons.Scanner.Scanner(function=transpFortranScannerFunction,name="transpFortranScanner",
                                             skeys=transpFortranSuffixes,
                                             path_function = SCons.Scanner.FindPathDirs("FPPPATH"),  #FindPathDirs("FPPPATH",fs=SCons.Node.FS.default_fs),
                                             recursive=1, scan_check=scan_check)

def newFortranBuilder(env, isdebug=0):
    """
    Build new static and shared object builders for compiling transp fortran files.
      env     = environment
      isdebug = nonzero for debug builders
    """
    action    = {}   # map suffix to action
    actionsh  = {}   # map suffix to action for shared support
    emitter   = {}   # map suffix to emitter
    emittersh = {}   # map suffix to emitter for shared support
    for x in transpFortranSuffixes:
        if (isdebug):
            action[x]    = transpFortranDebugAction
            actionsh[x]  = transpFortranDebugActionSh
        else:
            action[x]    = transpFortranOptAction
            actionsh[x]  = transpFortranOptActionSh
            
        emitter[x]   = transpFortranEmitter
        emittersh[x] = transpShFortranEmitter
        
    static = env.Builder(action = action,
                         emitter = emitter,
                         prefix = '$OBJPREFIX',
                         suffix = '$OBJSUFFIX',
                         #src_builder = ['CFile', 'CXXFile'],
                         source_scanner = transpFortranScanner,
                         single_source = 1)

        
    shared = env.Builder(action = actionsh,
                         emitter = emittersh,
                         prefix = '$SHOBJPREFIX',
                         suffix = '$SHOBJSUFFIX',
                         #src_builder = ['CFile', 'CXXFile'],
                         source_scanner = transpFortranScanner,
                         single_source = 1)
    return (static,shared)

#
# --------------------- C ------------------------
# create new builders for C,C++ so that CCFLAGS is used for the normal build
# and CCFLAGS_DEBUG is used for the debug build.
#

class transpC:
    """
    For some reason SCons does not want to expand a source file in the build directory to point
    to the build directory, instead it points to the original source directory.  Use this class
    to get around this and allow source files in the build directory.  Using this class for
    normal C builds also for anticipated flexibility.
    """
    def __init__(self,com,comstr,ibreak=0):
        """
        com    = command string
        comstr = description string
        ibreak = nonzero to break the rules
        """
        self.com    = com
        self.comstr = comstr
        self.ibreak = ibreak

    def actionFunction(self,target,source,env):
        if (self.ibreak):
            rn = source[0]                               # source node
            ra = rn.abspath                              # full path to source file
            c = env.subst(self.com,target=target)+" "+ra # breaking the rules
        else:
            c = env.subst(self.com,source=source[0],target=target[0])
        stat = os.system(c)

        return stat

    def descr(self,target,source,env):
        if (self.ibreak):
            rn = source[0]                               # source node
            ra = rn.abspath                              # full path to source file
            c = env.subst(self.comstr,target=target)
            if (not c):
                c = env.subst(self.com,target=target)
            return c+" "+ra
        else:
            c = env.subst(self.comstr,target=target,source=source)
            return c
        
    def getAction(self):
        return SCons.Action.Action(self.actionFunction, self.descr)
    

transpCOptAction     = transpC("$CCCOM", "$CCCOMSTR").getAction()
transpCOptActionSH   = transpC("$SHCCCOM", "$SHCCCOMSTR").getAction()

transpCDebugAction   = transpC("$CCCOM_DEBUG", "$CCCOMSTR_DEBUG", 1).getAction()     # SCons.Action.Action("$CCCOM_DEBUG", "$CCCOMSTR_DEBUG")
transpCDebugActionSH = transpC("$SHCCCOM_DEBUG", "$SHCCCOMSTR_DEBUG", 1).getAction() # SCons.Action.Action("$SHCCCOM_DEBUG", "$SHCCCOMSTR_DEBUG")

transpCXXOptAction   = transpC("$CXXCOM", "$CXXCOMSTR").getAction()
transpCXXOptActionSH = transpC("$SHCXXCOM", "$SHCXXCOMSTR").getAction()

transpCXXDebugAction   = transpC("$CXXCOM_DEBUG", "$CXXCOMSTR_DEBUG",1).getAction()     # SCons.Action.Action("$CXXCOM_DEBUG", "$CXXCOMSTR_DEBUG")
transpCXXDebugActionSH = transpC("$SHCXXCOM_DEBUG", "$SHCXXCOMSTR_DEBUG",1).getAction() #SCons.Action.Action("$SHCXXCOM_DEBUG", "$SHCXXCOMSTR_DEBUG")

def newCBuilder(env, isdebug=0):
    """
    Build new static and shared object builders for compiling transp C,C++ files.
      env     = environment
      isdebug = nonzero for debug builders
    """
    action  = {}   # map suffix to action
    emitter = {}   # map suffix to emitter
    for x in transpCSuffixes:
        if (isdebug):
            action[x]  = transpCDebugAction
        else:
            action[x]  = transpCOptAction
            
        emitter[x] = SCons.Defaults.StaticObjectEmitter
        
    for x in transpCXXSuffixes:
        if (isdebug):
            action[x]  = transpCXXDebugAction
        else:
            action[x]  = transpCXXOptAction
            
        emitter[x] = SCons.Defaults.StaticObjectEmitter
        
    static = env.Builder(action = action,
                         emitter = emitter,
                         prefix = '$OBJPREFIX',
                         suffix = '$OBJSUFFIX',
                         source_scanner = SCons.Tool.SourceFileScanner,
                         single_source = 1)

        
    action  = {}   # map suffix to action
    emitter = {}   # map suffix to emitter
    for x in transpCSuffixes:
        if (isdebug):
            action[x]  = transpCDebugActionSH
        else:
            action[x]  = transpCOptActionSH
            
        emitter[x] = SCons.Defaults.SharedObjectEmitter
        
    for x in transpCXXSuffixes:
        if (isdebug):
            action[x]  = transpCXXDebugActionSH
        else:
            action[x]  = transpCXXOptActionSH
            
        emitter[x] = SCons.Defaults.SharedObjectEmitter
        
    shared = env.Builder(action = action,
                         emitter = emitter,
                         prefix = '$SHOBJPREFIX',
                         suffix = '$SHOBJSUFFIX',
                         source_scanner = SCons.Tool.SourceFileScanner,
                         single_source = 1)
    return (static,shared)



#
# ----------------- linking -----------------
#
re_comment = re.compile(r'^\s*#')
re_tag     = re.compile(r'^\s*\$(\w+)')

class TranspLinkActionClass:
    """
    encapsulates the linking action common to fortran, C++ and C linkers
    """
    def __init__(self, name, linktype, isftnsrc=0):
        """
        linktype = name of linker, 'fortran', 'c++' else it will be 'c'
        """
        self.name     = name        # name of linker
        self.linktype = linktype    # fortran, c++ or c
        self.isftnsrc = isftnsrc    # nonzero if the main source file is fortran, only needed
                                    # when the linktype is c++
        if (linktype == "fortran"):
            self.sdescr = "Fortran" # string describing the linking
        elif (linktype == "c++"):
            if (isftnsrc):
                self.sdescr = "Fortran source with C++"
            else:
                self.sdescr = "C++"
        else:
            self.sdescr = "C"
        
    def action(self, target, source, env):
        """
        Action for linking with the fortran compiler.  The object files, distributed libraries
        and the <name>_exe.link file are passed in the source arguemnt.  The <name>_exe.link file
        is parsed to get the external libraries and possible trailers.
        """
        info = int(env.get("LINKLOAD_DEBUG","0")) >0         # true to print out info during each step

        ta = target[0].abspath         # target executable

        if (info):
            print "%s: %s"%(self.name,ta)

        flink   = None                 # _exe.link file
        objects = []                   # list of object files and static libraries
        for x in source:
            xa = x.abspath
            ext = os.path.splitext(xa)[1]
            if (ext == ".link"):
                flink = xa
            else:
                objects.append(xa)
            
        # -- get support libraries --
        has_cpp = 0
        has_ftn = 0
        has_ftn_main = 0
        l_libs = []

        if (not flink):
            # this is ok
            for x in external_packages:    # add all external packages which are defined
                u = x.upper()
                if (env.has_key("L_"+u)):
                    l_libs.append("$_L_"+u)
            #raise RuntimeError("did not find a '.link' file for linking %s"%ta)
        else:
            f = open(flink,'r')
            while(1):
                line = f.readline()
                if (not line):
                    break
                if (re_comment.match(line)):
                    continue

                m = re_tag.match(line)
                if (not m):
                    continue
                name = m.group(1).upper()
                if (name == "CPP"):
                    has_cpp = 1
                elif (name == "FTN"):
                    has_ftn = 1
                elif (name == "FTN_MAIN"):
                    has_ftn_main = 1
                else:
                    if (len(name)<3 or name[0:2]!="L_"):
                        raise RuntimeError("did not recognize the parameter %s in %s"%(name,flink))
                    l_libs.append("$_"+name)
            f.close()
        
        #if (l_libs):
        #    l_libs.insert(0, "$_TRLIB_PATH")   # add library path if there are external libraries

        l_libs.append("$LINK_LIBS")            # for stuff like OSX "-framework veclib"

        t_libs = []    # trailer libs
        # -- add in trailers, select flags --
        if (self.linktype == "fortran"):
            # fortran
            t_libs.append("$LINK_FORTRAN_TRAILER")
            if (has_cpp):
                t_libs.append("$CPP_TRAILER")
            flags  = "$LINK_FORTRANFLAGS $LINKFLAGS"
            linker = "$_LINK_FORTRAN"
        elif (self.linktype == "c++"):
            # c++
            if (self.isftnsrc or has_ftn):
                t_libs.append("$FTN_TRAILER")
                if (self.isftnsrc or has_ftn_main):
                    l_libs.insert(0,"$FTN_MAIN")        
            t_libs.append("$LINK_CXX_TRAILER")
            flags  = "$LINK_CXXFLAGS $LINKFLAGS"
            linker = "$_LINK_CXX"
        else:
            # c
            if (has_ftn):
                t_libs.append("$FTN_TRAILER")
                if (has_ftn_main):
                    l_libs.insert(0,"$FTN_MAIN")
            t_libs.append("$LINK_CC_TRAILER")
            flags  = "$LINK_CCFLAGS $LINKFLAGS"
            linker = "$_LINK_CC"

        rawcom = "%s %s -o $TARGET %s $_LIBDIRFLAGS %s $_LIBFLAGS %s"%(linker,flags,string.join(objects),string.join(l_libs),string.join(t_libs))
        com    = env.subst(rawcom,source=source,target=target)
    
        if (info):
            print "   ->raw command: ",rawcom
            print "   ->command:     ", com

        stat = os.system(com)   # do the link

        if (stat and not info):
            print "   ->raw command: ",rawcom
            print "   ->command:     ", com
            
        return stat
      
    def descr(self, target, source, env):
        """
        Returns a string describing the action
        """
        ta = target[0].abspath         # target executable

        return "%s linker: %s"%(self.sdescr,ta)


def newProgramBuilder(env, name, linktype, isftnsrc=0):
    """
    Build a new builder for linking the transp way.
      name     = name of linker for debug messages
      linktype = "fortran", "c++" or "c"
      isftnsrc = nonzero to force compiling in the fortran libraries and main
                 when the linktype is c++
    """
    c = TranspLinkActionClass(name, linktype,isftnsrc)
    a = c.action
    d = c.descr
    builder = env.Builder(action = env.Action(a,d),
                          prefix = '$PROGPREFIX',
                          suffix = '$PROGSUFFIX',
                          single_source = 0)
    return builder
        

#
# -------------------------- debug methods --------------------------
#
def transpDebugStaticLibraryFunction(target, source, env):
    """
    Action for building the static debug library.  The nondebug library will be
    copied to the target then all of the objects in the source will be used
    to replace the nondebug objects
    """
    dpath = target[0].abspath    # path to debug library to be built
    t = os.path.basename(dpath)

    libget = env.subst(r"$LIBPREFIX(.*)$LIBSUFFIX")  # look for lib<name>.a and extract <name>
    m = re.match(libget,t)
    if (not m):
        raise RuntimeError("could not get the library name from the target %s"%t)

    libname = m.group(1)
    
    transp_libraries = env['TRANSP_LIBRARIES']
    if (not transp_libraries.has_key(libname)):
        raise RuntimeError("did not find the library %s among the currently distributed transp libraries"%libname)

    opath = transp_libraries[libname]  # path of original distributed library

    shutil.copyfile(opath,dpath)  # copy the original to the new debug

    if (len(source)>0):
        arcom = env.subst("$ARCOM",target=target,source=source)
        stat = os.system(arcom)
        if (stat!=0):
            os.unlink(dpath)
        
    return stat


def transpDebugStaticLibraryDescr(target, source, env):
    """
    Description for building the static debug library.  The nondebug library will be
    copied to the target then all of the objects in the source will be used
    to replace the nondebug objects
    """
    dpath = target[0].abspath    # path to debug library to be built
    t = os.path.basename(dpath)

    libget = env.subst(r"$LIBPREFIX(.*)$LIBSUFFIX")
    m = re.match(libget,t)
    if (not m):
        raise RuntimeError("could not get the library name from the target %s"%t)

    libname = m.group(1)

    transp_libraries = env['TRANSP_LIBRARIES']
    if (not transp_libraries.has_key(libname)):
        raise RuntimeError("did not find the library %s among the currently distributed transp libraries"%libname)

    opath = transp_libraries[libname]  # path of original distributed library

    s = "copy %s %s"%(opath,dpath)
    if (len(source)>0):
        s+="\n"+env.subst("$ARCOM",target=target,source=source)
    
    return s


def newTranspDebugStaticLibraryBuilder(env):
    """
    Return a new builder for building a debug static library.
    env = environment
    """
    action = env.Action(transpDebugStaticLibraryFunction,transpDebugStaticLibraryDescr)
    dbg = env.Builder(action  = action,
                      prefix  = '$LIBPREFIX',
                      suffix  = '$LIBSUFFIX',
                      src_suffix = '$OBJSUFFIX')

    return dbg


#
# ------------------------- tool methods ----------------------------
#

class VariableListGenerator:
    """
    Snatched from fortran.py
    """
    def __init__(self, *variablelist, **dict):
        self.variablelist = variablelist
        self.defvar       = ''
        if (dict.has_key("def")):
            self.defvar = dict['def']
    def __call__(self, env, target, source, for_signature=0):
        for v in self.variablelist:
            try: return env[v]
            except KeyError: pass
        return self.defvar
    
def generate(env):
    # -- snatched from SCons.Tool.fortran.py --
    env['_FORTRANINCFLAGS'] = '$( ${_concat(INCPREFIX, FORTRANPATH, INCSUFFIX, __env__, RDirs, TARGET)} $)'
    env['_FORTRANMODFLAG']  = '$( ${_concat(FORTRANMODDIRPREFIX, FORTRANMODDIR, FORTRANMODDIRSUFFIX, __env__)} $)'
    env['_FORTRANDEFFLAGS'] = '$( ${_defines(CPPDEFPREFIX, FPPDEFINES, CPPDEFSUFFIX, __env__)} $)'
    env['_FPPINCFLAGS']     = '$( ${_concat(INCPREFIX, FPPPATH, INCSUFFIX, __env__, RDirs, TARGET)} $)'

    env['FORTRAN_FREECOM']  = '$FORTRAN $FORTRANFLAGS $FORTRAN_FREE $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'
    env['FORTRAN_FIXEDCOM'] = '$FORTRAN $FORTRANFLAGS $FORTRAN_FIXED $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'

    env['FORTRAN_FREECOM_DEBUG']  = '$FORTRAN $FORTRANFLAGS_DEBUG $FORTRAN_FREE $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'
    env['FORTRAN_FIXEDCOM_DEBUG'] = '$FORTRAN $FORTRANFLAGS_DEBUG $FORTRAN_FIXED $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'

    env['FORTRAN_FREECOM_SH']  = '$FORTRAN $FORTRANFLAGS $FORTRAN_SHARE $FORTRAN_FREE $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'
    env['FORTRAN_FIXEDCOM_SH'] = '$FORTRAN $FORTRANFLAGS $FORTRAN_SHARE $FORTRAN_FIXED $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'

    env['FORTRAN_FREECOM_DEBUG_SH']  = '$FORTRAN $FORTRANFLAGS_DEBUG $FORTRAN_SHARE $FORTRAN_FREE $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'
    env['FORTRAN_FIXEDCOM_DEBUG_SH'] = '$FORTRAN $FORTRANFLAGS_DEBUG $FORTRAN_SHARE $FORTRAN_FIXED $_FORTRANINCFLAGS $_FORTRANMODFLAG -c -o $TARGET $SOURCES'

    tstatic, tshared = newFortranBuilder(env,isdebug=0)
    env.Prepend(BUILDERS = {'StaticTranspFortran':tstatic, 'SharedTranspFortran':tshared})

    dstatic, dshared = newFortranBuilder(env,isdebug=1)
    env.Prepend(BUILDERS = {'StaticTranspDebugFortran':dstatic, 'SharedTranspDebugFortran':dshared})

    # --- C,C++ ---
    env['_CPPINCFLAGS'] = '$( ${_concat(INCPREFIX, CPPPATH, INCSUFFIX, __env__, RDirs, TARGET)} $)'

    env['CCCOM']   = '$CC $CCFLAGS $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'
    env['SHCCCOM'] = '$SHCC $SHCCFLAGS $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'

    env['CCCOM_DEBUG']   = '$CC $CCFLAGS_DEBUG $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'
    env['SHCCCOM_DEBUG'] = '$SHCC $SHCCFLAGS_DEBUG $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'

    env['CXXCOM']     = '$CXX $CXXFLAGS $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'
    env['SHCXXCOM']   = '$SHCXX $SHCXXFLAGS $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'

    env['CXXCOM_DEBUG']   = '$CXX $CXXFLAGS_DEBUG $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'
    env['SHCXXCOM_DEBUG'] = '$SHCXX $SHCXXFLAGS_DEBUG $CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES'

    env['CCCOMSTR']   = '$CCCOM'
    env['SHCCCOMSTR'] = '$SHCCCOM'

    env['CCCOMSTR_DEBUG']   = '$CCCOM_DEBUG'
    env['SHCCCOMSTR_DEBUG'] = '$SHCCCOM_DEBUG'

    env['CXXCOMSTR']     = '$CXXCOM'
    env['SHCXXCOMSTR']   = '$SHCXXCOM'

    env['CXXCOMSTR_DEBUG']   = '$CXXCOM_DEBUG'
    env['SHCXXCOMSTR_DEBUG'] = '$SHCXXCOM_DEBUG'

    env['CXXFLAGS_DEBUG']   = SCons.Util.CLVar('$CCFLAGS_DEBUG')

    tstatic, tshared = newCBuilder(env,isdebug=0)
    env.Prepend(BUILDERS = {'StaticTranspC':tstatic, 'SharedTranspC':tshared})

    dstatic, dshared = newCBuilder(env,isdebug=1)
    env.Prepend(BUILDERS = {'StaticTranspDebugC':dstatic, 'SharedTranspDebugC':dshared})

    # -- add builder --
    #static_obj, shared_obj = SCons.Tool.createObjBuilders(env)
    #for suffix in transpFortranSuffixes:
    #    static_obj.add_action(suffix,transpFortranAction)
    #    static_obj.add_emitter(suffix,transpFortranEmitter)
    #
    # -- add scanner --
    #static_scanner = static_obj.source_scanner
    #
    #for suffix in transpFortranSuffixes:
    #    static_scanner.add_scanner(suffix,transpFortranScanner)

    #print static_obj.source_scanner.dict
    #env['LIBDIRPREFIX'] = '-L'
    #env['LIBDIRSUFFIX'] = ''

    #env['_TRLIB_PATH']  = '$( ${_concat(LIBDIRPREFIX, TRLIB_PATH, LIBDIRSUFFIX, __env__)} $)'

    #env['LIBLINKPREFIX']='-l'
    #env['LIBLINKSUFFIX']=''

    # create L_NETCDF type of definitions
    for x in external_packages:
        z = x.upper()
        env['_L_'+z] = '${_stripixes(LIBLINKPREFIX, L_%s, LIBLINKSUFFIX, LIBPREFIX, LIBSUFFIX, __env__)}'%z

    env['_LINK_CC']      = VariableListGenerator('LINK_CC',      'CC',      defvar='gcc')
    env['_LINK_CXX']     = VariableListGenerator('LINK_CXX',     'CXX',     defvar='g++')
    env['_LINK_FORTRAN'] = VariableListGenerator('LINK_FORTRAN', 'FORTRAN', defvar='gfortran')  # good luck with the default

    env.Append(BUILDERS = {'ProgramTranspFortran':newProgramBuilder(env,"TranspFortran","fortran"),
                           'ProgramTranspCPP':newProgramBuilder(env,"TranspC++","c++"),
                           'ProgramTranspFortranCPP':newProgramBuilder(env,"TranspFortranC++","c++",1),
                           'ProgramTranspCC':newProgramBuilder(env,"TranspC","c"),
                           'TranspDebugStaticLibrary':newTranspDebugStaticLibraryBuilder(env)})
def exists(env):
    return 1
