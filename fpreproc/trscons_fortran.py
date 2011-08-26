#
# SCons tool for loading in a fortran compiler
#
import os
import trscons_lf95
import trscons_nag

lahey = "LaheyFujitsu"
nag   = "NagWare"

# map name to compiler tool
fcomp = {lahey:trscons_lf95, 
         nag:trscons_nag}

def generate(env):
  """
	env - environment
  """
  fenv = os.environ.get('FORTRAN_VARIANT',None)
	
  if (not fenv):
    workstation = os.environ.get('WORKSTATION_TYPE',"").strip()
    if (workstation=="OSX"):
      fenv = find_fortran(env, [nag])
    elif(workstation=="LINUX"):
      fenv = find_fortran(env,[lahey, nag])
    else:
      fenv = find_fortran(env, fcomp.keys())   # try all of them
	
  if (fcomp.has_key(fenv)):
    fcomp[fenv].generate(env)
  else:
    if (fenv):
      raise RuntimeError("Did not recognize the fortran compiler "+fenv)
    else:
      raise RuntimeError("Unable to find a FORTRAN compiler to load")
  
def find_fortran(env, prefs):
  """
  look for a fortran compiler in the order of prefs and return the name.
    env   - environment
    prefs - list of fortran variant compiler names to try
  """
  for x in prefs:
    if (fcomp.has_key(x)):
      if (fcomp[x].exists(env)):
        return x
  return None

def exists(env):
  return None
