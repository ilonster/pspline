#
# SCons tool for finding and loading blas.
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
    if (env['PLATFORM']=='darwin'):
        env.AppendUnique(FRAMEWORKS=["veclib"])
    else:
        if (env.has_key("L_BLAS")):
          return      # assume found
    
        l_blas = "blas"

        # - look for library -
        checkdir = env.get("BLAS_LIBRARY_PATH",None)
        lsearch = transpSConsUtils.findDirectory(env,l_blas,env['LD_LIBRARY_PATH'],
                                                 checkdir=checkdir,type=3)
        if (lsearch):
          t_blas = os.path.realpath(lsearch[0])
        else:
          raise ValueError("unable to find the blas library")
        env['L_BLAS'] = l_blas
        env.AppendUnique(LIBPATH=[t_blas])

        #print "env['L_BLAS']  =",env['L_BLAS']
        #print "env['LIBPATH'] =",env['LIBPATH']
        #print "env['CPPPATH'] =",env['CPPPATH']
    
def exists(env):
    return 1
