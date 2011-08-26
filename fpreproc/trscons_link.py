#
# Select one of the built in linkers based on PLATFORM variable
#

import SCons.Tool.gnulink
import SCons.Tool.applelink

def generate(env):
  """
  env - environment
  """
  if (env['PLATFORM']=='darwin'):
    SCons.Tool.applelink.generate(env)
    env['SHLINKFLAGS']='$LINKFLAGS -dynamiclib -single_module -flat_namespace '
  else:
    SCons.Tool.gnulink.generate(env)
    
    
def exists(env):
  return None
  