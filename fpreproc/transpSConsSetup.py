#
# Tool for defining the environment used to compile TRANSP using SCons
# and the transpSConsBuilder tool.  This tool takes some of the variables defined
# by the user in the transpPreSetup tool to construct all the environment variables
# needed by the builder.
#

import os
import transpSConsBuilder
import transpSConsUtils
import sys
import string

def generate(env):
    # --- shell environment variables ---
    code_debug = os.environ.get('CODESYS_DEBUG','0')   # set to a XYZ number for fortran compile messages
    try:
        code_debug = int(code_debug)
    except ValueError, e:
        code_debug = code_debug.strip().lower()
        if (code_debug=="no" or code_debug=="off" or code_debug=="" or code_debug=="false"):
            code_debug=0
        else:
            code_debug = 111
    
    link_debug = os.environ.get('LINKLOAD_DEBUG','0')  # set to a number for link messages
    try:
        link_debug = int(link_debug)
    except ValueError, e:
        link_debug = link_debug.strip().lower()
        if (link_debug=="no" or link_debug=="off" or link_debug=="" or link_debug=="false"):
            link_debug=0
        else:
            link_debug = 1

    workstation = os.environ.get('WORKSTATION_TYPE',"").strip()
    if (len(workstation)==0):
        plat = sys.platform
        if (string.find(plat, "linux") !=-1):
            workstation="LINUX"
        elif (string.find(plat, "darwin") !=-1):
            workstation="OSX"
        elif (string.find(plat, "sun") !=-1):
            workstation="SUN"
        elif (string.find(plat, "hp") !=-1):
            workstation="HP"
        else:
            raise ValueError("no WORKSTATION_TYPE environment variable and can't figure it out")
    
    env['WORKSTATION'] = workstation

    # --- default compilers ---
    if not transpSConsUtils.loadIfNone(env,"FORTRAN","gfortran"):
        raise AttributeError("unable to load in a FORTRAN environment")
    
    if not transpSConsUtils.loadIfNone(env,"CC","gcc"):
        raise AttributeError("unable to load in a CC (c) environment")
    
    if not transpSConsUtils.loadIfNone(env,"CXX","g++"):
        raise AttributeError("unable to load in a CXX (c++) environment")

    if not transpSConsUtils.loadIfNone(env,"LINK","link"):
        raise AttributeError("unable to load in a CXX (c++) environment")

    if not transpSConsUtils.loadIfNone(env,"AR","ar"):
        raise AttributeError("unable to load in a CXX (c++) environment")    
                                
    #
    # --- external libraries ---
    # external libraries should append to
    #   CPPDEFINES, CPPPATH, LIBPATH
    # these and L_*** must be setup for the external libraries prior to this Tool
    #
    nomdsplus = ""
    mpi       = "__MPI"
    for ext in transpSConsBuilder.external_packages:
        lname = "L_"+ext
        if (not env.has_key(lname)):
            if (ext == "MDSPLUS"):
                nomdsplus = '__NOMDSPLUS'
            elif (ext == "MPI"):
                mpi = ""
            else:
                pass
                # raise AttributeError("environment L_%s was not defined for the external library"%ext)
            env[lname]=""
            continue
        
    # -- preprocessor --
    env['FPPDEFINES']=[ x for x in env.get('CPPDEFINES',[])]  # copy as a new list
    
    cppargs = ["__"+workstation,"GETLINE_EDITOR"]  # more C preprocessor flags
    if (nomdsplus):
        cppargs.append(nomdsplus)
        
    fppargs = cppargs + ["__F90","__NODLINES","__UNIX"]   # more fortran preprocessor flags

    env.AppendUnique(FPPDEFINES=fppargs,CPPDEFINES=cppargs)

    # -- paths --
    publicmods = "#/%s/build/mods"%workstation
    
    incpath    = ["#/incl_cpp","#/inclshare",".",publicmods]+env.get('CPPPATH',[])   # additional CPPPATH for TRANSP stuff

    fpppath = incpath    # fortran preprocessor
    cpppath = incpath    # C preprocessor
    forpath = incpath    # fortran compiling

    env['CPPPATH']     = cpppath
    env['FPPPATH']     = fpppath
    env['FORTRANPATH'] = forpath

    # -- other --
    cppsuffixes = [".c",".C",".cxx", ".cpp", ".c++", ".cc", ".h", ".H", ".hpp", ".hh", ".F", ".f90", ".f", ".for"]

    env['CPPSUFFIXES'] = cppsuffixes

    env['FORTRANEXPORT']    = []      # list of source files with public modules, added to in SConscriptLibrary
    env['TRANSP_LIBRARIES'] = {}      # map library name to installed library, added to in SConscriptLibrary

    if (not env.has_key('FORTRANFLAGS_DEBUG')):
        env['FORTRANFLAGS_DEBUG'] = "$FORTRAN_FLAGS -g"

    if (not env.has_key('FORTRANMODSUFFIX')):
        env['FORTRANMODSUFFIX']=".mod"

    if (not env.has_key('CCFLAGS_DEBUG')):
        env['CCFLAGS_DEBUG'] = "$CCFLAGS -g"

    if (not env.has_key('CXXFLAGS_DEBUG')):
        env['CXXFLAGS_DEBUG'] = "$CXXFLAGS -g"

    env['FORTRANPUBLIC']=publicmods
    if (not env.has_key('FORTRANCPP')):
        env['FORTRANCPP']=0

    if (not env.has_key('SHCCFLAGS_DEBUG')):
        env['SHCCFLAGS_DEBUG'] = "$SHCCFLAGS -g"

    if (not env.has_key('CXXFLAGS_DEBUG')):
        env['SHCXXFLAGS_DEBUG'] = "$SHCXXFLAGS -g"

    env['FORTRANPUBLIC']=publicmods
    if (not env.has_key('FORTRANCPP')):
        env['FORTRANCPP']=0

    # -- trailers --
    ftn_libpath = env.get('FTN_LIBPATH',"")
    ftn_libs    = env.get('FTN_LIBS',[])
    ftn_main    = env.get('FTN_MAIN',"")
    if (ftn_libs):
        if (ftn_libpath):
            ftn_trailer = '${_concat(LIBDIRPREFIX, FTN_LIBPATH, LIBDIRSUFFIX, __env__)}'
        else:
            ftn_trailer = ""
        ftn_trailer += ' ${_stripixes(LIBLINKPREFIX, FTN_LIBS, LIBLINKSUFFIX, LIBPREFIX, LIBSUFFIX, __env__)}'
    else:
        ftn_trailer = os.environ.get('FTN_TRAILER',"")
    ftn_trailer = "$( %s $)"%ftn_trailer
    if (not ftn_main):
        ftn_main = os.environ.get('FTN_MAIN',"")
    ftn_main = "$( %s $)"%ftn_main
    
    env['FTN_MAIN']   =ftn_main
    env['FTN_TRAILER']=ftn_trailer

    cpp_libpath = env.get('CPP_LIBPATH',[])
    cpp_libs    = env.get('CPP_LIBS',[])
    cpp_objects = env.get('CPP_OBJECTS',[])
    if (cpp_libs):
        if (cpp_libpath):
            cpp_trailer = '${_concat(LIBDIRPREFIX, CPP_LIBPATH, LIBDIRSUFFIX, __env__)}'
        else:
            cpp_trailer = ""
        cpp_trailer += ' ${_stripixes(LIBLINKPREFIX, CPP_LIBS, LIBLINKSUFFIX, LIBPREFIX, LIBSUFFIX, __env__)}'
        for x in cpp_objects:
            cpp_trailer += " "+x
    else:
        cpp_trailer = os.environ.get('CPP_TRAILER',"")
    cpp_trailer = "$( %s $)"%cpp_trailer
    
    env['CPP_TRAILER']=cpp_trailer

    # -- script debug --
    env['CODESYS_DEBUG']  = code_debug
    env['LINKLOAD_DEBUG'] = link_debug

    # -- shared --
    if (env.has_key('FIX_SHARE') and env['FIX_SHARE']):
        env['SHOBJSUFFIX'] = '$OBJSUFFIX'
        env['SHOBJPREFIX'] = '$OBJPREFIX'

    #
    # --- load builders ---
    #
    transpSConsBuilder.generate(env)  

    if (code_debug):
        for x in ['WORKSTATION','ToolPath','CPPDEFINES','CPPPATH',
                  'FORTRAN','FORTRANFLAGS','FORTRANFLAGS_DEBUG','FORTRAN_FREE','FORTRAN_FIXED',
                  'FORTRAN_FREE_EXT','FORTRAN_FIXED_EXT','FORTRAN_MODSUFFIX','FORTRANPATH','FPPDEFINES','FPPPATH',
                  'CC', 'CCFLAGS', 'CCFLAGS_DEBUG',
                  'CXX', 'CXXFLAGS', 'CXXFLAGS_DEBUG']:
            print '%20s  = %s'%(x,env.get(x,""))

    if (link_debug):
        for ext in transpSConsBuilder.external_packages:
            lname = "L_"+ext
            print "%20s = %s"%(lname,str(env.get(lname,"")))

        for x in ['LIBPATH','FTN_MAIN','FTN_LIBS','FTN_LIBPATH','FTN_TRAILER',
                  'CPP_LIBS', 'CPP_LIBPATH', 'CPP_OBJECTS', 'CPP_TRAILER',
                  'LINKFLAGS',
                  'LINK_FORTRAN','LINK_FORTRAN_TRAILER','LINK_FORTRANFLAGS',
                  'LINK_CC','LINK_CCFLAGS','LINK_CC_TRAILER',
                  'LINK_CXX','LINK_CXXFLAGS','LINK_CXX_TRAILER']:
            if (x.lower().find('trailer')!=-1):
                print '%20s  = %s'%(x,env.subst(env.get(x,"")))
            else:
                print '%20s  = %s'%(x,env.get(x,""))
                
    if (code_debug):
        for x in ['FORTRAN_FREECOM','FORTRAN_FIXEDCOM','FORTRAN_FREECOM_DEBUG','FORTRAN_FIXEDCOM_DEBUG',
                  'CCCOM','CCCOM_DEBUG',
                  'CXXCOM', 'CXXCOM_DEBUG']:
            print '%20s  = %s'%(x,env.subst(env.get(x,"")))



def exists(env):
    return 1
