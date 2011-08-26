#
# SCons tool for loading in the NagWare compiler.  Includes additional definitions
# for TRANSP.  Snatched from SCons ifort.py
#
import os
import SCons
import SCons.Defaults
import SCons.Tool.fortran
import transpSConsUtils

def generate(env):
    """Add Builders and construction variables for NAG fortran to an Environment."""
    SCons.Tool.fortran.generate(env)      # defines various fortran flags

    env['_FORTRAND'] = 'f95'

    # --- transp stuff ---
    normpath = env.WhereIs('f95')
    if (not normpath):
        raise RuntimeError("could not find f95 in the path")
    
    normpath = os.path.normpath(os.path.realpath(normpath))
    dirpath  = os.path.dirname(normpath)

    env['FORTRAN']            = "f95" 
    env['FORTRANFLAGS']       = "-O -PIC -w=all -dcfuns -dusty -kind=byte -mismatch_all -maxcontin=85"
    env['FORTRANFLAGS_DEBUG'] = "-g -gline -PIC -O0 -dcfuns -dusty -w=all -kind=byte -mismatch_all -maxcontin=85"
    env['FORTRANMODSUFFIX']   = ".mod"
    env['FORTRAN_FREE_EXT']   = "f90"
    env['FORTRAN_FIXED_EXT']  = "for"
    env['FORTRAN_VARIANT']    = "NagWare"
    
    libpath = os.path.join(os.path.split(dirpath)[0],"lib")  # guess
    l2 = os.path.join(libpath,"NAGWare")
    if (os.path.exists(l2)):
      libpath=l2
    #print "nag guess = ",libpath
    fdir = transpSConsUtils.findDirectory(env,"f97",env['LD_LIBRARY_PATH'],
                                          checkdir=libpath,type=3)
    if (fdir):
        libpath=fdir[0]
        env['FTN_LIBPATH']=libpath
        env['FTN_LIBS']=['f97','f96','m']

    mainpath = os.path.join(libpath,"quickfit.o")
    if (os.path.isfile(mainpath)):
        env['FTN_MAIN']=mainpath

    fdir = transpSConsUtils.findDirectory(env,"f98",env['LD_LIBRARY_PATH'],
                                          checkdir=libpath,type=3)   
    if (fdir):
        libpath=fdir[0]
        env['FTN_SHLIBPATH']=libpath
        env['FTN_SHLIBS']=['extra_rob','f98']
        
def exists(env):
    return (env.Detect('f95') and env.Detect('f95mcheck'))
