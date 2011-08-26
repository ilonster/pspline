#
# This SCons Tool is executed after transpSConsSetup and is meant to modify
# the final environment variables used in the builders.  This is probably
# only useful for debugging.
#
# The environment variable TRANSP_SCONS_DIR can be used to add one or
# more directories to the tool path which is used to find all of the tools.
#
def generate(env):
    pass

def exists(env):
    return 1
