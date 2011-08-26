#
# SCons tool for loading in the gcc compiler.  Includes additional definitions
# for TRANSP.
#
import SCons
import SCons.Tool
import SCons.Tool.gcc

def generate(env):
    """Add Builders and construction variables for gcc to an Environment."""
    SCons.Tool.gcc.generate(env)
    
    env['CCFLAGS']       = '-O'
    env['CCFLAGS_DEBUG'] = '-g'
    
    env['SHCCFLAGS']       = '$CCFLAGS -fPIC'
    env['SHCCFLAGS_DEBUG'] = '$CCFLAGS_DEBUG -fPIC'

def exists(env):
    return Scons.Tool.gcc.exists(env)
