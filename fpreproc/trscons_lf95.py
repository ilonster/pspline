#
# SCons tool for loading in the lf95 compiler.  Includes additional definitions
# for TRANSP.  Snatched from SCons ifort.py
#
import os
import SCons
import SCons.Defaults
import SCons.Tool.fortran
import transpSConsUtils

def generate(env):
    """Add Builders and construction variables for lf95 to an Environment."""
    SCons.Tool.fortran.generate(env)      # defines various fortran flags

    env['_FORTRAND'] = 'lf95'

    # --- transp stuff ---
    ftype = 32
    normpath = env.WhereIs('lf95')
    if (not normpath):
        #print "no lf95"
        env['_FORTRAND'] = 'lfc'
        ftype = 64
        normpath = env.WhereIs('lfc')
        if (not normpath):
            print "no lfc"
            raise RuntimeError("could not find lf95 or lfc in the path")
    
    normpath = os.path.normpath(os.path.realpath(normpath))
    dirpath  = os.path.dirname(normpath)

    if (ftype==32):
        env['FORTRAN']        = "lf95" 
    else:
        env['FORTRAN']        = "lfc" 
        
    env['FORTRANFLAGS']       = "-O --trap --trace"
    env['FORTRANFLAGS_DEBUG'] = "-g --trap --trace"
    env['FORTRANMODSUFFIX']   = ".mod"
    env['FORTRAN_FREE_EXT']   = "f90"
    env['FORTRAN_FIXED_EXT']  = "for"
    env['FORTRAN_VARIANT']    = "LaheyFujitsu"

    if (ftype==32):
        libpath = os.path.join(os.path.split(dirpath)[0],"lib")  # guess
        fdir = transpSConsUtils.findDirectory(env,"fj9i6",env['LD_LIBRARY_PATH'],
                                              checkdir=libpath,type=3)
    else:
        libpath = os.path.join(os.path.split(dirpath)[0],"lib64")  # guess
        fdir = transpSConsUtils.findDirectory(env,"fj90i",env['LD_LIBRARY_PATH'],
                                              checkdir=libpath,type=3)
        
    if (fdir):
        libpath=fdir[0]
        env['FTN_LIBPATH']=libpath
        if (ftype==32):
            env['FTN_LIBS']=['fj9i6', 'fj9f6', 'fj9e6', 'fst', 'fccx86_6a']
            env['FTN_EXTRA_LIBS']=['fst']  # needed on fortran link line
        else:
            env['FTN_LIBS']=['fj90i', 'fj90f']
            lsearch = transpSConsUtils.findDirectory(env,"elf-0.97.1",env['LD_LIBRARY_PATH'],
                                                     checkdir="/usr/lib",type=3)
            if (lsearch==None):
                raise RuntimeError("could not find elf library")
            env.AppendUnique(LIBPATH=lsearch[0])
            env.AppendUnique(LIBS="elf")
            
        env['FTN_SHLIBPATH']=env['FTN_LIBPATH']
        env['FTN_SHLIBS']=env['FTN_LIBS']

        env['LINK_FORTRAN_TRAILER'] = '${_concat(LIBDIRPREFIX, FTN_LIBPATH, LIBDIRSUFFIX, __env__)} ' + \
                                      '${_stripixes(LIBLINKPREFIX, FTN_EXTRA_LIBS, LIBLINKSUFFIX, LIBPREFIX, LIBSUFFIX, __env__)}'
    mainpath = os.path.join(libpath,"fj90rt0.o")
    if (os.path.isfile(mainpath)):
        env['FTN_MAIN']=mainpath
        
def exists(env):
    return env.Detect('lf95') or env.Detect('lfc')
