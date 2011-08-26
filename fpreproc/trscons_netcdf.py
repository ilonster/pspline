#
# SCons tool for finding and loading netcdf.
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
    if (env.has_key("L_NETCDF")):
        return      # assume found
    
    l_netcdf = "netcdf"

    # - look for library -
    checkdir = env.get("NETCDF_LIBRARY_PATH",None)
    lsearch = transpSConsUtils.findDirectory(env,l_netcdf,env['LD_LIBRARY_PATH'],
                                            checkdir=checkdir,type=3)
    if (lsearch):
        t_netcdf = os.path.realpath(lsearch[0])
    else:
        raise ValueError("unable to find the netcdf library")

    w_netcdf = [l_netcdf]  # library names
    x_netcdf = [t_netcdf]  # library paths

    # - look for includes -
    i_netcdf = []
    checkdir=env.get("NETCDF_INCLUDE_PATH",
                     os.path.join(os.path.split(lsearch[0])[0],"include"))  # guess
    for inc in ["netcdf.h","netcdf.inc"]:
        isearch = transpSConsUtils.findDirectory(env,inc,env['INCLUDE_PATH'],
                                                 checkdir=checkdir)
        if (isearch):
            if (checkdir != isearch[0]):
                checkdir += " "+isearch[0]
            x = os.path.realpath(isearch[0])
            if (x not in i_netcdf):
                i_netcdf.append(x)
        else:
            raise ValueError("unable to find the netcdf include file "+inc)

    # check for Lors
    if (t_netcdf.find('netcdfL')>=0):
        # assume this is a lors build of netcdf
        for x in ['xio','lors_all','xml2']:
            
            lsearch = transpSConsUtils.findDirectory(env,x,env['LD_LIBRARY_PATH'],
                                                     checkdir=t_netcdf,type=3)
            w_netcdf.append(x)
            if (lsearch):
                x_netcdf.append(os.path.realpath(lsearch[0]))
            else:
                raise ValueError("unable to find the %s library for a lors netcdf installation"%x)       
        

    env['L_NETCDF'] = w_netcdf
    env.AppendUnique(LIBPATH=x_netcdf, CPPPATH=i_netcdf)

    #print "env['L_NETCDF']=",env['L_NETCDF']
    #print "env['LIBPATH'] =",env['LIBPATH']
    #print "env['CPPPATH'] =",env['CPPPATH']
    
def exists(env):
    return 1
