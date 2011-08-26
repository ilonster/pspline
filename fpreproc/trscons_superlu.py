#
# SCons tool for finding and loading superlu.
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
import trscons_blas

def generate(env):
    """Add construction variables for an external module to an Environment."""
    if (env.has_key("L_SUPERLU")):
        return      # assume found
    
    # - dependent library -
    trscons_blas.generate(env)
    
    l_superlu = "superlu"

    # - look for library -
    checkdir = env.get("SUPERLU_LIBRARY_PATH",None)
    if (checkdir==None):
        checkdir = os.environ.get("SUPERLU_ROOT",None)
        if (checkdir!=None):
            checkdir = os.path.join(checkdir.strip(),"lib")
            
    lsearch = transpSConsUtils.findDirectory(env,l_superlu,env['LD_LIBRARY_PATH'],
                                             checkdir=checkdir,type=3)
    if (lsearch):
        t_superlu = os.path.realpath(lsearch[0])
    else:
        raise ValueError("unable to find the superlu library")

    # - look for includes -
    i_superlu = []
    checkdir=env.get("SUPERLU_INCLUDE_PATH",
                     os.path.join(os.path.split(lsearch[0])[0],"include"))  # guess
    for inc in ["slu_ddefs.h"]:
        isearch = transpSConsUtils.findDirectory(env,inc,env['INCLUDE_PATH'],
                                                 checkdir=checkdir)
        if (isearch):
            if (checkdir != isearch[0]):
                checkdir += " "+isearch[0]
            x = os.path.realpath(isearch[0])
            if (x not in i_superlu):
                i_superlu.append(x)
        else:
            raise ValueError("unable to find the superlu include file "+inc)

    env['L_SUPERLU'] = [ l_superlu ] + env.Split(env['L_BLAS'])
    
    env.AppendUnique(LIBPATH=[t_superlu], CPPPATH=i_superlu)

    #print "env['L_SUPERLU']  =",env['L_SUPERLU']
    #print "env['LIBPATH']    =",env['LIBPATH']
    #print "env['CPPPATH']    =",env['CPPPATH']
    
def exists(env):
    return 1
