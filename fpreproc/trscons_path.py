#
# This SCons Tool sets up the LD_LIBRARY_PATH and INCLUDE_PATH lists
# in the environment for use in library searches
#
import os
import string

def generate(env):
    # --- LD_LIBRARY_PATH ---
    spath = os.environ.get("LD_LIBRARY_PATH","").strip()  
    if (len(spath)==0):
        spath = os.environ.get("DYLD_LIBRARY_PATH","").strip()   # mac

    snames = spath.split(os.pathsep)
    lpath = [ s for s in snames if len(s)>0 ]
            
    if (os.name in ['posix','mac']):
        lpath.append('/usr/local/lib')
        lpath.append('/usr/lib')
        lpath.append('/lib')

    # make unique
    uname = {}
    ld_library_path = []
    for s in lpath:
        x = s.strip()
        if (x not in uname):
            ld_library_path.append(x)
            uname[x] = 1
    
    env['LD_LIBRARY_PATH'] = ld_library_path

    # --- INCLUDE_PATH ---
    spath  = os.environ.get("INCLUDE_PATH","").strip()  
    snames = spath.split(os.pathsep)
    
    spath  = os.environ.get("C_INCLUDE_PATH","").strip()  
    snames.extend(spath.split(os.pathsep))

    ipath = [ s for s in snames if len(s)>0 ]
            
    if (os.name in ['posix','mac']):
        ipath.append('/usr/local/include')
        ipath.append('/usr/include')

    # make unique
    uname = {}
    include_path = []
    for s in ipath:
        x = s.strip()
        if (x not in uname):
            include_path.append(x)
            uname[x] = 1
            
    env['INCLUDE_PATH'] = include_path

def exists(env):
    return 1
