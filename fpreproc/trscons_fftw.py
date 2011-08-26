#
# SCons tool for finding and loading fftw.
#
# The loading of an external library <ext> requires setting,
#
#    L_<ext> = list of library names (without prefix 'lib' or suffix '.a')
#              needed during linking
# and appending uniquely with env.AppendUnique() to,
#
#    CPPDEFINES = preprocessor definitions.  This should be a list of strings 'name'
#                 and tuples ('name','value') which will be converted to -Dname and -Dname=value
#    CPPPATH    = directory or list of directories where the #include files are located
#    LIBPATH    = directory or list of directories where the libraries are located
#
# The default external library loaders will search for libraries in
#   <ext>_LIBRARY_PATH, LD_LIBRARY_PATH
# and include files in
#   <ext>_INCLUDE_PATH, INCLUDE_PATH
# It will not change the environment if L_<ext> is already loaded
#
import SCons
import transpSConsUtils
import os
import os.path

def generate(env):
    """Add construction variables for an external module to an Environment."""
    if (env.has_key("L_FFTW")):
        return      # assume found
    
    l_fftw = "fftw"

    # - look for library -
    checkdir = env.get("FFTW_LIBRARY_PATH",None)
    lsearch = transpSConsUtils.findDirectory(env,l_fftw,env['LD_LIBRARY_PATH'],
                                             checkdir=checkdir,type=3)
    if (lsearch):
        t_fftw = os.path.realpath(lsearch[0])
    else:
        raise ValueError("unable to find the fftw library")

    # - look for includes -
    i_fftw = []
    checkdir=env.get("FFTW_INCLUDE_PATH",
                     os.path.join(os.path.split(lsearch[0])[0],"include"))  # guess
    for inc in ["fftw.h"]:
        isearch = transpSConsUtils.findDirectory(env,inc,env['INCLUDE_PATH'],
                                                 checkdir=checkdir)
        if (isearch):
            if (checkdir != isearch[0]):
                checkdir += " "+isearch[0]
            x = os.path.realpath(isearch[0])
            if (x not in i_fftw):
                i_fftw.append(x)
        else:
            raise ValueError("unable to find the fftw include file "+inc)

    env['L_FFTW'] = l_fftw
    env.AppendUnique(LIBPATH=[t_fftw], CPPPATH=i_fftw)

    #print "env['L_FFTW']  =",env['L_FFTW']
    #print "env['LIBPATH'] =",env['LIBPATH']
    #print "env['CPPPATH'] =",env['CPPPATH']
    
def exists(env):
    return 1
