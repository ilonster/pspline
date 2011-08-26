#/usr/bin/python
#
#  python version of postfpp (experimental, dmc Jan 2005) -- see also
#  prefpp.py ... (transcribed from codesys/source/postfpp.f90)
#
#  postfpp "decodes" prefpp/gcc processed fortran source code so that it can
#  be fed safely to a fortran compiler.
#
#  for more information please see the prefpp.py comments...
#----------------------------------------------------------------------------

import sys
import string

oldr = "BOF"

flines = sys.stdin.readlines()

for aline in flines:
    line = string.strip(aline)
    if len(line) == 0:
        continue
    
    imatch = 0
    for lib in sys.argv:
        ldr = "source/" + lib + "/"
        if line[0:len(ldr)] == ldr:
            imatch = imatch + 1

    if imatch > 0:
        continue

    pieces = string.split(line,"/")
    if len(pieces) < 2:
        ldr = pieces[0]
    else:
        ldr = pieces[0] + "/" + pieces[1]

    if ldr == oldr:
        print line
    else:
        if oldr != "BOF":
            print " "
        oldr = ldr
        print line
