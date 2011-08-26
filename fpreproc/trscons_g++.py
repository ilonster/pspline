#
# SCons tool for loading in the g++ compiler.  Includes additional definitions
# for TRANSP.
#
import os
import re
import SCons
gpp = __import__('SCons.Tool.g++', globals(), locals(), [])
gpp = getattr(gpp,'Tool')
gpp = getattr(gpp,'g++')

re_colon = re.compile("^([^:]+):(.*)")

def getInfo(env):
    """
    Fetch some info from gcc by running it and return in a dictionary.
      env = environment
    in particular look at result['install'] which appears to be the install directory for gcc
    """
    fcmd = "gcc -print-search-dirs"
    f = os.popen(fcmd,'r')                   # read output from stdout of gcc
    gout = f.readlines()                     # grab lines of preprocessed output
    f.close()

    result = {}
    for s in gout:
        m = re_colon.match(s)    # only look at line with a colon
        if (m):
            k,v = m.groups()
            result[k.strip()]=v.strip()
            
    return result
        
def getPath(env,name):
    """
    Fetch path to a gcc file from gcc by running it.
      env  = environment
      name = name of file to find
    in particular look at result['install'] which appears to be the install directory for gcc
    """
    name = name.strip()
    if (name=="gcc"):
        fcmd = "gcc -print-libgcc-file-name"
    else:
        fcmd = "gcc -print-file-name=%s"%name
    f = os.popen(fcmd,'r')                   # read output from stdout of gcc
    gout = f.readlines()                     # grab lines of preprocessed output
    f.close()

    if (len(gout)<1):
        return None
    out = gout[0].strip()
    if (out==name):
        return None
    return os.path.realpath(out)

def getLibPath(env,name):
    """
    Fetch path to a gcc library by undecorated library name
      env = environment
      name = library base name
    """
    name = name.strip()
    if (name=="gcc"):
        return getPath(env,name)
    x = env.subst('${LIBPREFIX}%s${LIBSUFFIX}'%name).strip()
    p = getPath(env,x)
    if (p==None):
        x = env.subst('${SHLIBPREFIX}%s${SHLIBSUFFIX}'%name)
        p = getPath(env,x)
    if (p==None):
        return None
    return p.strip()
    
def generate(env):
    """Add Builders and construction variables for g++ to an Environment."""
    gpp.generate(env)
    
    env['CXXFLAGS']       = '-O'
    env['CXXFLAGS_DEBUG'] = '-g'

    env['SHCXXFLAGS']       = '$CXXFLAGS -fPIC'
    env['SHCXXFLAGS_DEBUG'] = '$CXXFLAGS_DEBUG -fPIC'

    # -- look for trailer --
    result = getInfo(env)
    if ('install' in result):
        idir = os.path.realpath(result['install'])    # gcc install directory

        cpp_libpath = [idir]      # directories to libraries
        cpp_libs    = []          # libraries
        for x in ['stdc++','gcc','gcc_s']:
            fpath = getLibPath(env,x)
            if (fpath):
                d = os.path.realpath(os.path.split(fpath)[0])  # directory containing library
                cpp_libs.append(x)
                if (d not in cpp_libpath):
                    cpp_libpath.append(d)

        cpp_objects = []          # companion object files
        for x in ['crtbegin.o','crtend.o','crtn.o']:
            p = getPath(env,x)
            if (p):
                cpp_objects.append(p.strip())

        env['CPP_LIBPATH'] = cpp_libpath
        env['CPP_LIBS']    = cpp_libs
        env['CPP_OBJECTS'] = cpp_objects
        
        #print "CPP_LIBPATH = ",env['CPP_LIBPATH']
        #print "CPP_LIBS    = ",env['CPP_LIBS']
        #print "CPP_OBJECTS = ",env['CPP_OBJECTS']

def exists(env):
    return gpp.exists(env)

