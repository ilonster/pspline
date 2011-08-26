#
# This SCons Tool is executed before transpSConsSetup and should load in the
# environment values for the fortran,C,C++ compilers and external libraries.
# This particular file is the default and can be used as a guide for making
# a site specific version.  The environment variable TRANSP_SCONS_DIR can
# be used to add one or more directories to the tool path which is used to
# find all of the tools.
#
def generate(env):
    toolpath = env.get("ToolPath","")  # path to tools includes TRANSP_SCONS_SITE
    
    #
    # --- Linker ---
    # select one of the built in linkers based on PLATFORM variable
    #
    env.Tool("trscons_link",toolpath)
    
    #
    # --- Fortran compiler ---
    # define,
    #   FORTRAN  - name of executable for the fortran compiler.  If this is
    #              not set then a default fortran compiler (gfortran which
    #              probably won't work) will be loaded.
    #   FTN_MAIN - name of object file containing the fortran main.  This is
    #              is used when linking with the c++ compiler.  If not set
    #              then the FTN_MAIN environment variable or nothing will be used.
    #   FTN_LIBS - list of fortran language libraries.  This is used when linking
    #              with the c++ compiler.  If not set then the FTN_TRAILER environment
    #              variable will be used.
    #   FTN_LIBPATH - path or list of paths to FTN_LIBS.  Ignored if FTN_LIBS is
    #              not set
    #   FORTRANFLAGS - flags for regular fortran compiling
    #   FORTRANFLAGS_DEBUG - flags for debug compiling.  If this is not set then
    #                        FORTRANFLAGS will be used with the "-g" option
    #
    env.Tool("trscons_fortran",toolpath)

    #
    # --- C compiler ---
    # define,
    #  CC - name of executable for C compiler.  If this is not set then a default
    #       C compiler (gcc) will be loaded.
    #  CCFLAGS - flags for regular C compiling
    #  CCFLAGS_DEBUG - flags for debug compiling.  If this is not set then CCFLAGS
    #                  will be used with the "-g" option
    #
    env.Tool("trscons_gcc",toolpath)

    #
    # --- C++ compiler ---
    # define,
    #  CXX - name of executable for C++ compiler.  If this is not set then a default
    #        C++ compiler (g++) will be loaded.
    #  CXXFLAGS - flags for regular C++ compiling
    #  CXXFLAGS_DEBUG - flags for debug compiling.  If this is not set then CXXFLAGS
    #                   will be used with the "-g" option 
    #  CPP_TRAILER - this will be added to the link line when compiling a fortran
    #                executable which contains C++ code.  If not set the CPP_TRAILER
    #                environment variable or nothing (good luck with that) will be used.
    #
    env.Tool("trscons_g++",toolpath)

    #
    # --- External Libraries ---
    # External libraries like netcdf, mdsplus must define the environment variables
    #
    #    L_<ext> = list of library names (without prefix 'lib' or suffix '.a')
    #              needed during linking
    #
    # and appending uniquely with env.AppendUnique() to,
    #
    #    CPPDEFINES = preprocessor definitions.  This should be a list of strings 'name'
    #                 and tuples ('name','value') which will be converted to -Dname and -Dname=value
    #    CPPPATH    = directory or list of directories where the #include files are located
    #    LIBPATH    = directory or list of directories where the libraries are located
    #
    # The default tools loaded below attempt to find the libraries in
    # the LD_LIBRARY_PATH path and the include files in INCLUDE_PATH, C_INCLUDE_PATH
    # 

    # - netcdf -
    env.Tool("trscons_netcdf",toolpath)
    
    # - mdsplus -
    env.Tool("trscons_mdsplus",toolpath)   # ok if this fails
    
    # - fftw -
    env.Tool("trscons_fftw",toolpath)
    
    # - rfftw -
    env.Tool("trscons_rfftw",toolpath) 
    
    # - blas -
    env.Tool("trscons_blas",toolpath) 
    
    # - lapack -
    env.Tool("trscons_lapack",toolpath) 
    
    # - superlu -
    env.Tool("trscons_superlu",toolpath) 

    env['SHOBJSUFFIX'] = '$OBJSUFFIX'
    env['SHOBJPREFIX'] = '$OBJPREFIX'
    
def exists(env):
    return 1
